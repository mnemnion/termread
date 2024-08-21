//! # Termese: a focused terminal input parser
//!
//! 'Termese' is the language which terminals speak.  In the dark days
//! of the glass tty, terminals spoke in a profusion of languages, but
//! here in the grim cyberpunk future, they have settled into a lingua
//! franca.  Underdocumented, quirky, but nonetheless a common tongue.
//!
//! This library is a protocol droid, translating fragments of termese
//! into a structured form, suitable for use in applications.
//!
//! This is its sole remit.  It does not read or poll, it doesn't talk
//! to the terminal: it hears, and it tells.
//!
//! ## Design
//!
//! The ugly truth is that extant terminal emulators speak in a variety
//! of mostly-compatible dialects.  The first goal is to read the Kitty
//! protocol.  It is the 'high' tongue of terminals, spoken by the best
//! and brightest, and we may expect it to crowd out the low-down slang
//! of legacy terminal emulators.
//!
//! But never entirely.  The `termese` library should be able to handle
//! any plausible expression of the VT100 lineage.  When these are not,
//! as they are not, compatible, then we will add ways to configure the
//! reader to translate legacy slangs into our common parsed form.
//!
//! The library will also understand a form of written termese, perhaps
//! several.  These are statments like `<Del>`, or `<C-M-a>`, as can be
//! found in configurations of terminal-oriented programs.  These serve
//! for testing, as well as to make use of said configurations.
//!
//! # Interface
//!
//! The library intends to preserve the full richness of Termese, while
//! translating into a single and canonical form.
//!
//! This goal poses certain challenges.  For example, depending on what
//! enhancement level has been selected, Shift-a may be reported as the
//! shift key and `a`, or just as `A`.  How we solve this is TBD.  Part
//! of the answer is that the Termese interpreter will be configured to
//! know the expected format of keystrokes, so the program will already
//! know that e.g. the

const std = @import("std");
const assert = std.debug.assert;

// NOTE: must always be greater than MAX_ASSOCIATED_TEXT * 4
const BUF_SIZE = 116;
// Maximum number of 'associated text' codepoints to decode.
const MAX_ASSOCIATED_TEXT = 24;

/// Flags to handle ambiguous or inconsistent inputs
quirks: Quirks = Quirks{},
/// View buffer for info and associated text.  Do not
/// read directly, and information here will be included in
/// a TermReport.
buffer: [BUF_SIZE]u8 = .{0x20} ** BUF_SIZE,
/// Internal tracking of how much of the buffer has been
/// allocated.  Private, do not modify.
buf_len: usize = 0,

const TermRead = @This();

/// Configure Termese to recognize known quirks in terminal reporting.
/// This is a place for enduring differences of opinion in dialect, not
/// an all-purpose bugfix bucket.
pub const Quirks = packed struct(u8) {
    /// Terminal sends 0x08 for <BS>, 0x7f for <C-BS> / <Del>.
    backspace_as_delete: bool = false,
    /// ISO-keyboard-specific quirks.
    iso: bool = false,
    /// Reserved for other quirks.
    reserved: u6 = 0,
};

/// Read one sequence from `in` buffer.  Returns a `Reply`, containing
/// the status of the read, the report, and the remainder of the in
/// buffer.
pub fn read(term: *TermRead, in: []const u8) Reply {
    switch (in[0]) {
        // NUL is legacy for Ctrl-Space.
        0 => return Reply.ok(modText(mod().Control(), ' '), in[1..]),
        // Legacy ^X, for some values of X.
        0x01...0x07, 0x0a...0x0c, 0x0e...0x1a => |c| {
            // All reporting is lower case.
            return Reply.ok(modText(mod().Control(), c + 0x60), in[1..]);
        },
        0x08 => { // <Del>
            if (term.quirks.backspace_as_delete) {
                return Reply.ok(special(.backspace), in[1..]);
            } else {
                return Reply.ok(modKey(mod().Control(), specialKey(.backspace)), in[1..]);
            }
        },
        0x09 => return Reply.ok(special(.tab), in[1..]),
        // https://vt100.net/docs/vt100-ug/chapter3.html#T3-5
        0x1c => return Reply.ok(modText(mod().Control(), '\\'), in[1..]),
        0x1d => return Reply.ok(modText(mod().Control(), ']'), in[1..]),
        0x1e => return Reply.ok(modText(mod().Control(), '~'), in[1..]),
        0x1f => return Reply.ok(modText(mod().Control(), '?'), in[1..]),
        0x20 => return Reply.ok(text(' '), in[1..]),
        0x7f => { // <BS>
            if (term.quirks.backspace_as_delete) {
                return Reply.ok(modKey(mod().Control(), specialKey(.backspace)), in[1..]);
            } else {
                return Reply.ok(special(.backspace), in[1..]);
            }
        },
        0x1b => { // The Main Event!
            return term.parseEsc(in);
        },

        else => return term.parseText(in),
    }
}

fn parseEsc(term: *TermRead, in: []const u8) Reply {
    assert(in[0] == '\x1b');
    if (in.len == 1) {
        return Reply.ok(special(.esc), in);
    }
    switch (in[1]) {
        'O' => return term.parseSs3(in),
        '[' => return term.parseCsi(in),
        0x1b => { // Legacy Alt-Esc
            return Reply.ok(modKey(mod().Alt(), specialKey(.esc)), in[2..]);
        },
        'P' => {}, // A kind of reporting https://terminalguide.namepad.de/seq/csi_sw_t_dollar-1/
        else => { // Lead alt, probably.
            const reply = term.read(in[1..]);
            if (reply.status == .complete) {
                switch (reply.report) {
                    .key => |k| {
                        // Lead-alt is legacy, so we only care about modifier,
                        // and key:
                        var kmod = k.mod;
                        return Reply.ok(modKey(kmod.Alt(), k.value), reply.rest);
                    },
                    .paste,
                    .mouse,
                    .info,
                    .more,
                    .associated_text,
                    => {
                        // Plausible interpretation: Esc followed by something
                        // else.  Rather than try and figure it out, we report
                        // the escape and handle a reparse.
                        return Reply.ok(special(.esc), in[1..]);
                    },
                    .unrecognized, .malformed => return reply,
                }
            }
        },
    }
    unreachable;
}

fn parseSs3(term: *TermRead, in: []const u8) Reply {
    assert(in[0] == 0x1b);
    assert(in[1] == 'O');
    // If that's all we got, there may well be more coming:
    if (in.len == 2) return moreNeeded(false, in);

    switch (in[2]) {
        'A' => return Reply.ok(special(.up), in[3..]),
        'B' => return Reply.ok(special(.down), in[3..]),
        'C' => return Reply.ok(special(.right), in[3..]),
        'D' => return Reply.ok(special(.left), in[3..]),
        'H' => return Reply.ok(special(.home), in[3..]),
        'F' => return Reply.ok(special(.end), in[3..]),
        'P'...'S' => |fk| { // 'P' - 0x4f aka '0' == 1
            return Reply.ok(fKey(fk - 0x4f), in[3..]);
        },
        else => { // Valid esc code, don't know what
            return notRecognized(in[0..3], in[3..]);
        },
    }
    _ = term;
}

fn parseCsi(term: *TermRead, in: []const u8) Reply {
    const maybe_info = validateControlSequence(in);
    if (maybe_info == null) {
        return moreNeeded(false, in);
    } // That checked, we can do this:
    const info = maybe_info.?;
    if (!info.valid) {
        return malformedRead(info.stop, in);
    }
    const rest = in[info.stop..];
    const b = info.byte;
    // Csi-u?
    if (b == 'u') {
        if (!info.has_intermediates)
            return term.parseCsiU(in, in[2..info.stop], rest)
        else // Arguably unrecognized (private use and all),
            //  but not for our purposes
            return malformedRead(info.stop, in);
    }
    // Case of no modifiers.
    if (info.stop == 3) {
        switch (b) { // Various legacy special keys
            'A' => return Reply.ok(special(.up), rest),
            'B' => return Reply.ok(special(.down), rest),
            'C' => return Reply.ok(special(.right), rest),
            'D' => return Reply.ok(special(.left), rest),
            'E' => return Reply.ok(keyPad(.KP_Begin), rest),
            'H' => return Reply.ok(special(.home), rest),
            'F' => return Reply.ok(special(.end), rest),
            'Z' => return Reply.ok(modKey(mod().Shift(), specialKey(.tab)), rest),
            else => return notRecognized(in[0..info.stop], rest),
        }
    }
    if (std.mem.indexOfScalar(u8, "~ABCDEFHPQS", b)) |_| {
        return term.parseModifiedCsi(info, in);
    }
    // Handle other CSI sequences we might see
    switch (b) {
        'M' => {}, // Mouse
        'Z' => {
            // Support fixterms Ctrl-Shift-Tab
            if (std.mem.eql(u8, in[2..6], "1;5")) {
                return Reply.ok(modKey(mod().Shift().Control(), specialKey(.tab)), rest);
            } else { // There are some sequences here, probably not worth recognizing
                return notRecognized(in[0..info.stop], rest);
            }
        },
        'R' => {}, // cursor origin report https://terminalguide.namepad.de/seq/csi_sn-6/
        '?' => {}, // Queries
        else => return notRecognized(in[0..info.stop], rest),
    }
    if (info.is_private_use) {
        assert('<' <= in[2] and in[2] <= '?');
        switch (in[2]) {
            '<' => {},
            '=' => {},
            '>' => {},
            '?' => {}, // Various sorts of réportage
            else => unreachable,
        }
    }
    // Many options due to reporting of various sorts, TBD
    unreachable; // Will be..
}

const UTF_MAX = 0x10ffff;

fn parseCsiU(term: *TermRead, in: []const u8, seq: []const u8, rest: []const u8) Reply {
    // Preconditions
    assert(in[0] == '\x1b');
    assert(in[1] == '[');
    assert(in[2] == seq[0]);
    assert(seq[seq.len - 1] == 'u');
    // Structure:
    // unicode-key-code[:shifted-key[:base-layout-key]][;modifiers[:event-type][;text-as-codepoints]]u
    // CSI validated, 'u' is known.
    const seq_idx = 2 + seq.len;
    const codepoint, var idx = parseParameter(u21, seq) catch {
        return malformedRead(seq_idx, in);
    };
    if (codepoint > UTF_MAX) {
        // Overlarge codepoint
        return malformedRead(2 + seq.len, in);
    }
    if (idx == seq.len - 1) {
        // Easy
        return Reply.ok(text(codepoint), rest);
    }
    const key_val = keyFromCodepoint(codepoint);
    // Obtain shifted key, if available.
    const has_shifted = seq[idx] == ':';
    const shifted_key: u21 = shifted: {
        if (has_shifted) {
            idx += 1;
            if (seq[idx] == ':') {
                // Base key but no shift
                break :shifted 0;
            } else {
                const shift_point, const idx_delta = parseParameter(u21, seq[idx..]) catch {
                    return malformedRead(seq_idx, in);
                };
                if (shift_point > UTF_MAX) {
                    return malformedRead(seq_idx, in);
                }
                idx += idx_delta;
                break :shifted shift_point;
            }
        } else { // No cursor advance here (expect ';')
            break :shifted 0;
        }
    };
    // Obtain base key, if available.
    const base_key: u21 = base: {
        if (has_shifted) {
            if (seq[idx] == ':') {
                idx += 1;
                const base_point, const idx_delta = parseParameter(u21, seq[idx..]) catch {
                    return malformedRead(seq_idx, in);
                };
                if (base_point > UTF_MAX) {
                    return malformedRead(seq_idx, in);
                }
                idx += idx_delta;
                break :base base_point;
            } else {
                // ? assert(seq[idx] == ';') possible malformations though
                break :base 0;
            }
        } else break :base 0;
    };
    // Modifiers?
    var modifier = mod();
    if (seq[idx] == ';') {
        idx += 1;
        if (seq[idx] != ';') {
            var mod_value, const idx_delta = parseParameter(u9, seq[idx..]) catch {
                return malformedRead(seq_idx, in);
            };
            mod_value -= 1;
            if (mod_value <= std.math.maxInt(u8)) {
                const mod8: u8 = @intCast(mod_value);
                modifier = @bitCast(mod8);
            }
            idx += idx_delta;
        }
    } // Spec requires this to be consistent:
    if (shifted_key > 0 and !modifier.shift) return malformedRead(2 + seq.len, in);

    // Event type?
    const event: KeyEvent = event: {
        if (seq[idx] == ':') {
            idx += 1;
            const event_value, const idx_delta = parseParameter(u2, seq[idx..]) catch {
                return malformedRead(seq_idx, in);
            };
            idx += idx_delta;
            switch (event_value) {
                1 => break :event .press,
                2 => break :event .repeat,
                3 => break :event .release,
                0 => return malformedRead(2 + seq.len, in),
            }
        } else {
            break :event .press;
        }
    };
    // The protocol allows for any number of codepoints of associated text.
    // This is unreasonable, so we set a sensible limit.
    // Tracking issue: https://github.com/kovidgoyal/kitty/issues/7749
    //
    // Rather than allow a brain-damaged specification to DoS your
    // program by spamming unlimited associated text, this library
    // has decided to accept a generous buffer of up to 24 codepoints
    // of associated text, more than twice the size of the largest
    // actual grapheme (not Zalgo text) in Unicode 15.
    // More text than this will be ignored.
    if (seq[idx] == ';') {
        idx += 1;
        term.buf_len = 0;
        var code_count: usize = 0;
        while (code_count <= MAX_ASSOCIATED_TEXT) {
            const assoc_value, const idx_delta = parseParameter(u21, seq[idx..]) catch {
                return malformedRead(seq_idx, in);
            };
            if (assoc_value > UTF_MAX) {
                return malformedRead(seq_idx, in);
            }
            const wrote = std.unicode.wtf8Encode(assoc_value, term.buffer[term.buf_len..]) catch unreachable;
            term.buf_len += wrote;
            idx += idx_delta;
            if (seq[idx] != ':') break;
            idx += 1;
            code_count += 1;
        }
        // Drop anything else on the floor where it belongs
        // Entrance to this function requires a 'u' so this is legitimate.
        while (seq[idx] != 'u') : (idx += 1) {}
        return Reply{
            .status = .complete,
            .report = TermReport{
                .associated_text = AssociatedTextReport{
                    .key = KeyReport{
                        .value = key_val,
                        .event = event,
                        .mod = modifier,
                        .shifted = shifted_key,
                        .base_key = base_key,
                    },
                    .text = term.buffer[0..term.buf_len],
                },
            },
            .rest = rest,
        };
    }
    // Assertion: we read the entire sequence
    assert(idx == seq.len - 1);
    return Reply{
        .status = .complete,
        .report = TermReport{
            .key = KeyReport{
                .value = key_val,
                .event = event,
                .mod = modifier,
                .shifted = shifted_key,
                .base_key = base_key,
            },
        },
        .rest = rest,
    };
}

fn parseModifiedCsi(term: *TermRead, info: CsiInfo, in: []const u8) Reply {
    _ = term;
    assert(in[0] == '\x1b');
    assert(in[1] == '[');
    const rest = in[info.stop..];
    if (info.byte == '~') { // Legacy special keys
        const keyval, const idx_delta = parseParameter(u21, in[2..]) catch {
            return malformedRead(info.stop, in);
        };
        var modifier: KeyMod = mod();
        if (in[2 + idx_delta] == ';') {
            var mod_val, _ = parseParameter(u9, in[3 + idx_delta ..]) catch {
                return malformedRead(info.stop, in);
            };
            mod_val -= 1;
            if (mod_val <= std.math.maxInt(u8)) {
                const mod8: u8 = @intCast(mod_val);
                modifier = @bitCast(mod8);
            } else {
                return malformedRead(info.stop, in);
            } // TODO: needs an assert that we've read to the ~
        }
        const key: Key = switch (keyval) {
            2 => specialKey(.insert),
            3 => specialKey(.delete),
            5 => specialKey(.page_up),
            6 => specialKey(.page_down),
            15 => Key{ .f = 5 },
            // F6 - F12
            17...24 => |fk| Key{ .f = @intCast(fk - 11) },
            29 => specialKey(.menu),
            else => return malformedRead(info.stop, in),
        };
        return Reply.ok(modKey(modifier, key), rest);
    }
    // Get modifier one of two ways.
    var modifier: KeyMod = mod();
    if (in[3] == '1' and in[4] == ';') {
        var mod_val, _ = parseParameter(u9, in[5..]) catch {
            return malformedRead(info.stop, in);
        };
        mod_val -= 1;
        if (mod_val <= std.math.maxInt(u8)) {
            const mod8: u8 = @intCast(mod_val);
            modifier = @bitCast(mod8);
        } else {
            return malformedRead(info.stop, in);
        }
    } else if ('0' <= in[3] and in[3] <= '9') {
        var mod_val, _ = parseParameter(u9, in[3..]) catch {
            return malformedRead(info.stop, in);
        };
        mod_val -= 1;
        if (mod_val <= std.math.maxInt(u8)) {
            const mod8: u8 = @intCast(mod_val);
            modifier = @bitCast(mod8);
        } else {
            return malformedRead(info.stop, in);
        }
    }
    switch (info.stop) {
        'A' => return Reply.ok(modKey(modifier, specialKey(.up)), rest),
        'B' => return Reply.ok(modKey(modifier, specialKey(.down)), rest),
        'C' => return Reply.ok(modKey(modifier, specialKey(.right)), rest),
        'D' => return Reply.ok(modKey(modifier, specialKey(.left)), rest),
        'E' => return Reply.ok(modKey(modifier, kpKey(.KP_Begin)), rest),
        'H' => return Reply.ok(modKey(modifier, specialKey(.home)), rest),
        'F' => return Reply.ok(modKey(modifier, specialKey(.end)), rest),
        'P' => return Reply.ok(modKey(modifier, Key{ .f = 1 }), rest),
        'Q' => return Reply.ok(modKey(modifier, Key{ .f = 2 }), rest),
        'S' => return Reply.ok(modKey(modifier, Key{ .f = 4 }), rest),
        else => unreachable, // We checked this at function entrance
    }
}

fn parseText(term: *TermRead, in: []const u8) Reply {
    _ = term;
    const b = std.unicode.utf8ByteSequenceLength(in[0]) catch {
        return malformedRead(1, in);
    };
    const code = std.unicode.utf8Decode(in[0..b]) catch {
        return malformedRead(b, in);
    };
    return Reply.ok(text(code), in[b..]);
}

const ParameterError = error{
    BadParameter,
    ParameterTooBig,
};

// Parse a parameter into integer type T, returning the value and next index.
fn parseParameter(T: type, seq: []const u8) ParameterError!struct { T, usize } {
    // TODO: Throw an error here
    var i: usize = 0;
    while ('0' <= seq[i] and seq[i] <= '9') : (i += 1) {}
    if (i > 0) {
        const parsed_value: usize = std.fmt.parseInt(T, seq[0..i], 10) catch {
            return error.BadParameter;
        };
        if (parsed_value <= std.math.maxInt(T)) {
            const value: T = @intCast(parsed_value);
            return .{ value, i };
        } else {
            return error.ParameterTooBig;
        }
    } else @panic("parseParameter called on non-numeric sequence");
}

const CsiInfo = struct {
    /// Final byte
    byte: u8,
    /// in[0..stop] for sequence read
    stop: usize,
    /// Is CSI private use string?
    is_private_use: bool,
    /// Has an intermediate byte?
    has_intermediates: bool,
    /// Valid parameter string?
    valid: bool,
};

/// Verify the integrity of the CSI control sequence, returning the
/// final byte. or throwing an `InvalidCSI` error if the string is
/// not valid.  If the string is valid but no final byte is reached,
/// return `null` (partial command).  An out parameter is provided to
/// report the index of either an error or a found final byte.
///
/// References are to ECMA-48/1991.
fn validateControlSequence(in: []const u8) ?CsiInfo {
    // §5.4.1b
    const is_private_use = '<' <= in[2] and in[2] <= '?';
    if (is_private_use) return CsiInfo{
        .byte = 0xff,
        .stop = 3,
        .is_private_use = true,
        .has_intermediates = false,
        .valid = true, // So far as we know!
    };
    var intermediate = false;
    for (2..in.len) |i| {
        const b = in[i];
        if (!intermediate) {
            if (0x30 <= b and b <= 0x3b) {
                continue;
            } else if (0x20 <= b and b <= 0x2f) {
                intermediate = true;
                continue;
            }
        } else { // TODO: does anything actually use > 1 intermediate byte?
            if (0x20 <= b and b <= 0x2f) continue;
        } // Final byte? §5.4d)
        if (0x40 <= b and b <= 0x7e) {
            return CsiInfo{
                .byte = b,
                .stop = i + 1,
                .is_private_use = false,
                .has_intermediates = intermediate,
                .valid = true,
            };
        } else {
            return CsiInfo{
                .byte = b,
                .stop = i + 1,
                .is_private_use = false,
                .has_intermediates = intermediate, // Why not?
                .valid = false,
            };
        }
    }
    return null;
}

//| Builders.

fn text(char: u21) TermReport {
    return TermReport{ .key = KeyReport{
        .value = Key{
            .char = char,
        },
    } };
}

fn modKey(modifier: KeyMod, key: Key) TermReport {
    return TermReport{ .key = KeyReport{
        .value = key,
        .mod = modifier,
    } };
}

fn modText(modifier: KeyMod, char: u21) TermReport {
    return TermReport{ .key = KeyReport{
        .value = Key{
            .char = char,
        },
        .mod = modifier,
    } };
}

fn mod() KeyMod {
    return KeyMod{};
}

fn special(key_type: KeyTag) TermReport {
    return TermReport{ .key = KeyReport{
        .value = specialKey(key_type),
    } };
}

fn specialKey(key_type: KeyTag) Key {
    switch (key_type) {
        .backspace => return Key{ .backspace = {} },
        .enter => return Key{ .enter = {} },
        .left => return Key{ .left = {} },
        .right => return Key{ .right = {} },
        .up => return Key{ .up = {} },
        .down => return Key{ .down = {} },
        .home => return Key{ .home = {} },
        .end => return Key{ .end = {} },
        .page_up => return Key{ .page_up = {} },
        .page_down => return Key{ .page_down = {} },
        .tab => return Key{ .tab = {} },
        .back_tab => return Key{ .back_tab = {} },
        .delete => return Key{ .delete = {} },
        .insert => return Key{ .insert = {} },
        .esc => return Key{ .esc = {} },
        .caps_lock => return Key{ .caps_lock = {} },
        .scroll_lock => return Key{ .scroll_lock = {} },
        .num_lock => return Key{ .num_lock = {} },
        .print_screen => return Key{ .print_screen = {} },
        .pause => return Key{ .pause = {} },
        .menu => return Key{ .menu = {} },
        .f, .char, .keypad, .media, .modifier => unreachable,
    }
}

fn keyPad(key_type: KeyPadKey) TermReport {
    return TermReport{ .key = KeyReport{
        .value = kpKey(key_type),
    } };
}

fn kpKey(key_type: KeyPadKey) Key {
    return Key{
        .keypad = key_type,
    };
}

fn fKey(num: u21) TermReport {
    return TermReport{ .key = KeyReport{
        .value = Key{ .f = @intCast(num) },
    } };
}

fn notRecognized(seq: []const u8, rest: []const u8) Reply {
    return Reply{
        .status = .unrecognized,
        .report = TermReport{
            .unrecognized = .{ .sequence = seq },
        },
        .rest = rest,
    };
}

fn moreNeeded(is_paste: bool, in: []const u8) Reply {
    return Reply{
        .status = .more,
        .report = TermReport{
            .more = .{ .is_paste = is_paste },
        },
        .rest = in,
    };
}

fn malformedRead(idx: usize, in: []const u8) Reply {
    return Reply{
        .status = .malformed,
        .report = TermReport{
            .malformed = .{ .sequence = in[0..idx] },
        },
        .rest = in[idx..],
    };
}

//| Types

pub const Reply = struct {
    /// The status of the read.
    status: ReadStatus,
    /// Associated report.
    report: TermReport,
    /// Remainder of buffer (may be empty).
    rest: []const u8,

    /// Convenience function for 'normal' replies.
    pub fn ok(report: TermReport, rest: []const u8) Reply {
        return Reply{
            .status = .complete,
            .report = report,
            .rest = rest,
        };
    }

    pub fn format(
        reply: Reply,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        try writer.print("status: {s}\n", .{@tagName(reply.status)});
        try reply.report.format(fmt, options, writer);
        try writer.print("rest: '{s}'\n", .{reply.rest});
    }
};

/// Status of a read.
pub const ReadStatus = enum(u2) {
    /// We parsed a complete terminal event.
    complete,
    /// We were successfully parsing and require more input.
    more,
    /// A structurally valid, but unrecognized, sequence.
    unrecognized,
    /// Garbage.
    malformed,
};

/// Basic category of a read event.
pub const TermEventKind = enum(u4) {
    /// A key event: one or several keys pressed.
    key,
    /// A reply from the terminal to some request.
    info,
    /// Pasted data.
    paste,
    /// Mouse movement.
    mouse,
    /// More read is needed.
    more,
    /// A key report with associated text.
    associated_text,
    /// Unrecognized but valid sequence.
    unrecognized,
    /// Malformed sequence
    malformed,
};

/// Types of reported terminal event.
///
/// In the event of `Reply.status == .complete`, this will be one of
/// `.key`, `.info`, `.paste`, or `.mouse`.  The other three enums
/// represent unusual situations matching the corresponding status.
pub const TermReport = union(TermEventKind) {
    key: KeyReport,
    info: InfoReport,
    paste: Paste,
    mouse: MouseReport,
    more: MoreReport,
    associated_text: AssociatedTextReport,
    unrecognized: UnrecognizedReport,
    malformed: MalformedReport,

    pub fn format(
        term_report: TermReport,
        fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        switch (term_report) {
            .key => |k| {
                try writer.writeAll("key: ");
                try k.format(fmt, options, writer);
                try writer.writeAll("\n");
            },
            .info => {
                try writer.print("info: \n", .{});
            },
            .paste => {
                try writer.print("paste: \n", .{});
            },
            .mouse => {
                try writer.print("mouse: \n", .{});
            },
            .more => {
                try writer.print("more: \n", .{});
            },
            .associated_text => {
                try writer.print("associated text: \n", .{});
            },
            .unrecognized => {
                try writer.print("unrecognized: \n", .{});
            },
            .malformed => {
                try writer.print("malformed: \n", .{});
            },
        }
    }
};

pub const InfoReport = struct {
    string: []const u8,
}; // TODO: Stub

pub const Paste = struct {
    string: []const u8,
}; // TODO: Stub (albeit a fairly complete one)

pub const MoreReport = struct {
    is_paste: bool,
};

pub const UnrecognizedReport = struct {
    sequence: []const u8,
};

pub const MalformedReport = struct {
    sequence: []const u8,
};

pub const AssociatedTextReport = struct {
    key: KeyReport,
    text: []const u8,
};

pub const KeyReport = struct {
    mod: KeyMod = KeyMod{},
    value: Key,
    event: KeyEvent = .press,
    shifted: u21 = 0,
    base_key: u21 = 0,

    pub fn format(
        key: KeyReport,
        fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;
        const md = key.mod;
        if (md.capslock) try writer.writeAll("[Capslock] ");
        if (md.numlock) try writer.writeAll("[Numlock] ");
        const is_modded = md.isModified();
        if (is_modded) {
            try writer.writeAll("<");
            if (md.shift) try writer.writeAll("S-");
            if (md.alt) try writer.writeAll("A-");
            if (md.control) try writer.writeAll("C-");
            if (md.super) try writer.writeAll("D-");
            if (md.hyper) try writer.writeAll("H-");
            if (md.meta) try writer.writeAll("M-");
        }
        switch (key.value) {
            .backspace,
            .enter,
            .left,
            .right,
            .up,
            .down,
            .home,
            .end,
            .page_up,
            .page_down,
            .tab,
            .back_tab,
            .delete,
            .esc,
            .caps_lock,
            .scroll_lock,
            .num_lock,
            .print_screen,
            .pause,
            .menu,
            .insert,
            => try writer.print("<{s}>", .{@tagName(key.value)}),
            .f => |f| try writer.print("F{d}", .{f}),
            .char => |c| try writer.print("{u}", .{c}),
            .keypad => |k| try writer.print("KP_{u}", .{k.value(false)}),
            .media => |k| try writer.print("M_{s}", .{@tagName(k)}),
            .modifier => |k| try writer.print("<{s}>", .{@tagName(k)}),
        }
        if (is_modded) try writer.writeAll(">");
    }
};

/// Category of info reported.
pub const InfoKind = enum {
    unknown,
    operating,
    cursor_position,
    cell_size,
    pixel_size,
    cursor_style,
    // There's a lot of these...
};

/// Type of mouse press reported.
pub const MousePress = enum {
    left,
    middle,
    right,
    release,
    wheel_up,
    wheel_down,
    // Weirdo XTerm stuff
    wheel_left,
    wheel_right,
    button_8,
    button_9,
    button_10,
    button_11,
    none,
};

/// Modifier held during mouse report event.
pub const MouseModifier = enum(u2) {
    none,
    shift,
    meta,
    ctrl,
};

/// A report from the terminal regarding the mouse.
pub const MouseReport = struct {
    button: MousePress,
    mod: MouseModifier,
    col: u16,
    line: u16,
};

/// Type of key event.
pub const KeyEvent = enum(u2) {
    press = 1,
    repeat,
    release,
};

/// Modifiers of a key event.
pub const KeyMod = packed struct(u8) {
    shift: bool = false,
    alt: bool = false,
    control: bool = false,
    super: bool = false,
    hyper: bool = false,
    meta: bool = false,
    capslock: bool = false,
    numlock: bool = false,

    //| Reporting Functions

    /// Answer whether a modifier is set.  Doesn't count shift,
    /// capslock, or numlock.
    pub fn isModified(m: KeyMod) bool {
        return m.alt or m.control or m.super or m.hyper or m.meta;
    }

    /// Answer whether a key lock (caps or num) is set.
    pub fn isLocked(m: KeyMod) bool {
        return m.capslock or m.numlock;
    }

    //| Fluent builder functions

    pub fn Shift(m: KeyMod) KeyMod {
        var m1 = m;
        m1.shift = true;
        return m1;
    }

    pub fn Alt(m: KeyMod) KeyMod {
        var m1 = m;
        m1.alt = true;
        return m1;
    }

    pub fn Control(m: KeyMod) KeyMod {
        var m1 = m;
        m1.control = true;
        return m1;
    }

    pub fn Super(m: KeyMod) KeyMod {
        var m1 = m;
        m1.super = true;
        return m1;
    }

    pub fn Hyper(m: KeyMod) KeyMod {
        var m1 = m;
        m1.super = true;
        return m1;
    }

    pub fn Meta(m: KeyMod) KeyMod {
        var m1 = m;
        m1.meta = true;
        return m1;
    }

    pub fn Capslock(m: KeyMod) KeyMod {
        var m1 = m;
        m1.capslock = true;
        return m1;
    }

    pub fn Numlock(m: KeyMod) KeyMod {
        var m1 = m;
        m1.numlock = true;
        return m1;
    }
};

/// All possible Key types.
pub const KeyTag = enum(u5) {
    backspace,
    enter,
    left,
    right,
    up,
    down,
    home,
    end,
    page_up,
    page_down,
    tab,
    back_tab,
    delete,
    insert,
    f,
    char,
    keypad,
    esc,
    caps_lock,
    scroll_lock,
    num_lock,
    print_screen,
    pause,
    menu,
    media,
    modifier,
};

fn keyFromCodepoint(code: u21) Key {
    assert(code <= UTF_MAX);
    switch (code) {
        UCS.Esc => return Key.tab,
        UCS.Tab => return Key.esc,
        UCS.Bs => return Key.backspace,
        UCS.CapsLock => return Key.caps_lock,
        UCS.NumLock => return Key.num_lock,
        UCS.ScrollLock => return Key.scroll_lock,
        UCS.PrintScreen => return Key.print_screen,
        UCS.Pause => return Key.pause,
        UCS.Menu => return Key.menu,
        UCS.F13...UCS.F35 => |fk| return Key{ .f = @intCast(fk - UCS.F13 + 13) },
        UCS.KP_0...UCS.KP_Begin => |kpk| return Key{ .keypad = @enumFromInt(kpk - UCS.KP_0) },
        UCS.M_Play...UCS.MuteVolume => |mk| return Key{ .media = @enumFromInt(mk - UCS.M_Play) },
        UCS.LeftShift...UCS.ISOLevel5Shift => |modk| return Key{ .modifier = @enumFromInt(modk - UCS.LeftShift) },
        else => return Key{ .char = code },
    }
}

/// Represents a key press event.
pub const Key = union(KeyTag) {
    backspace,
    enter,
    left,
    right,
    up,
    down,
    home,
    end,
    page_up,
    page_down,
    tab,
    back_tab,
    delete,
    insert,
    f: u6,
    char: u21,
    keypad: KeyPadKey,
    esc,
    caps_lock,
    scroll_lock,
    num_lock,
    print_screen,
    pause,
    menu,
    media: MediaKey,
    modifier: ModifierKey,
};

/// Known types of Media key.
pub const MediaKey = enum(u4) {
    Play,
    Pause,
    PlayPause,
    Reverse,
    Stop,
    FastForward,
    Rewind,
    TrackNext,
    TrackPrevious,
    Record,
    LowerVolume,
    RaiseVolume,
    MuteVolume,
};

/// Known types of Modifier key.
pub const ModifierKey = enum(u4) {
    LeftShift,
    LeftControl,
    LeftAlt,
    LeftSuper,
    LeftHyper,
    LeftMeta,
    RightShift,
    RightControl,
    RightAlt,
    RightSuper,
    RightHyper,
    RightMeta,
    ISOLevel3Shift,
    ISOLevel5Shift,
};

/// Represents a keypad character press.
pub const KeyPadKey = enum(u5) {
    KP_0,
    KP_1,
    KP_2,
    KP_3,
    KP_4,
    KP_5,
    KP_6,
    KP_7,
    KP_8,
    KP_9,
    KP_Decimal,
    KP_Divide,
    KP_Multiply,
    KP_Subtract,
    KP_Add,
    KP_Enter,
    KP_Equal,
    KP_Separator,
    KP_Left,
    KP_Right,
    KP_Up,
    KP_Down,
    KP_PageUp,
    KP_PageDown,
    KP_Home,
    KP_End,
    KP_Insert,
    KP_Delete,
    KP_Begin,

    /// Return the value of the button pressed.  Some such
    /// values are from the Private Use Area and must be
    /// translated into a normalized form.
    pub fn value(key: KeyPadKey, iso_sep: bool) u21 {
        switch (key) {
            .KP_0 => return '0',
            .KP_1 => return '1',
            .KP_2 => return '2',
            .KP_3 => return '3',
            .KP_4 => return '4',
            .KP_5 => return '5',
            .KP_6 => return '6',
            .KP_7 => return '7',
            .KP_8 => return '8',
            .KP_9 => return '9',
            .KP_Decimal => {
                if (iso_sep) {
                    return ',';
                } else {
                    return '.';
                }
            },
            .KP_Divide => return '/',
            .KP_Multiply => return '*',
            .KP_Subtract => return '-',
            .KP_Add => return '+',
            .KP_Enter => return '\x0d', // \r, not \n
            .KP_Equal => return '=',
            .KP_Separator => {
                if (iso_sep) {
                    return '.';
                } else {
                    return ',';
                }
            },
            .KP_Left => return UCS.KP_Left,
            .KP_Right => return UCS.KP_Right,
            .KP_Up => return UCS.KP_Up,
            .KP_Down => return UCS.KP_Down,
            .KP_PageUp => return UCS.KP_PageUp,
            .KP_PageDown => return UCS.KP_PageDown,
            .KP_Home => return UCS.KP_Home,
            .KP_End => return UCS.KP_End,
            .KP_Insert => return UCS.KP_Insert,
            .KP_Delete => return UCS.KP_Delete,
            .KP_Begin => return UCS.KP_Begin,
        }
    }
};

/// Namespace for Unicode key values.  Kitty-focused.
/// These are CSI u codes, missing values in this table
/// are handled in some other fashion.
const UCS = struct {
    pub const Esc = 27;
    pub const Tab = 3;
    pub const Bs = 127;
    pub const CapsLock = 57358;
    pub const NumLock = 57360;
    pub const ScrollLock = 57359;
    pub const PrintScreen = 57361;
    pub const Pause = 57362;
    pub const Menu = 57363;
    pub const F13 = 57376;
    pub const F14 = 57377;
    pub const F15 = 57378;
    pub const F16 = 57379;
    pub const F17 = 57380;
    pub const F18 = 57381;
    pub const F19 = 57382;
    pub const F20 = 57383;
    pub const F21 = 57384;
    pub const F22 = 57385;
    pub const F23 = 57386;
    pub const F24 = 57387;
    pub const F25 = 57388;
    pub const F26 = 57389;
    pub const F27 = 57390;
    pub const F28 = 57391;
    pub const F29 = 57392;
    pub const F30 = 57393;
    pub const F31 = 57394;
    pub const F32 = 57395;
    pub const F33 = 57396;
    pub const F34 = 57397;
    pub const F35 = 57398;
    pub const KP_0 = 57399;
    pub const KP_1 = 57400;
    pub const KP_2 = 57401;
    pub const KP_3 = 57402;
    pub const KP_4 = 57403;
    pub const KP_5 = 57404;
    pub const KP_6 = 57405;
    pub const KP_7 = 57406;
    pub const KP_8 = 57407;
    pub const KP_9 = 57408;
    pub const KP_Decimal = 57409;
    pub const KP_Divide = 57410;
    pub const KP_Multiply = 57411;
    pub const KP_Subtract = 57412;
    pub const KP_Add = 57413;
    pub const KP_Enter = 57414;
    pub const KP_Equal = 57415;
    pub const KP_Separator = 57416;
    pub const KP_Left = 57417;
    pub const KP_Right = 57418;
    pub const KP_Up = 57419;
    pub const KP_Down = 57420;
    pub const KP_PageUp = 57421;
    pub const KP_PageDown = 57422;
    pub const KP_Home = 57423;
    pub const KP_End = 57424;
    pub const KP_Insert = 57425;
    pub const KP_Delete = 57426;
    pub const KP_Begin = 57427;
    pub const M_Play = 57428;
    pub const M_Pause = 57429;
    pub const M_PlayPause = 57430;
    pub const M_Reverse = 57431;
    pub const M_Stop = 57432;
    pub const M_FastForward = 57433;
    pub const M_Rewind = 57434;
    pub const M_TrackNext = 57435;
    pub const M_TrackPrevious = 57436;
    pub const M_Record = 57437;
    pub const LowerVolume = 57438;
    pub const RaiseVolume = 57439;
    pub const MuteVolume = 57440;
    pub const LeftShift = 57441;
    pub const LeftControl = 57442;
    pub const LeftAlt = 57443;
    pub const LeftSuper = 57444;
    pub const LeftHyper = 57445;
    pub const LeftMeta = 57446;
    pub const RightShift = 57447;
    pub const RightControl = 57448;
    pub const RightAlt = 57449;
    pub const RightSuper = 57450;
    pub const RightHyper = 57451;
    pub const RightMeta = 57452;
    pub const ISOLevel3Shift = 57453;
    pub const ISOLevel5Shift = 57454;
};

//| Tests
//

const talloc = std.testing.allocator;
const expect = std.testing.expect;
const expectEqual = std.testing.expectEqual;
const OhSnap = @import("ohsnap");

test "parsing" {
    const oh = OhSnap{};
    var term = TermRead{};
    {
        const reply = term.read("\x00");
        try expectEqual(.complete, reply.status);
        const key = reply.report.key;
        try expect(key.mod.control);
        try expectEqual(' ', key.value.char);
    }
    try oh.snap(
        @src(),
        \\
        ,
    ).showFmt(term.read("Qz"));
    try oh.snap(
        @src(),
        \\
        ,
    ).showFmt(term.read("\x1b\x1b"));
    try oh.snap(
        @src(),
        \\
        ,
    ).showFmt(term.read("\x1b[97;4u"));
    try oh.snap(
        @src(),
        \\
        ,
    ).showFmt(term.read("\x1b[B7;4u"));
}

test "KeyPadKey" {
    const kp_key = KeyPadKey.KP_2;
    try expectEqual('2', kp_key.value(false));
}

test "Associated Text" {
    const oh = OhSnap{};
    var term = TermRead{};
    try oh.snap(
        @src(),
        \\termese.Reply
        \\  .status: termese.ReadStatus
        \\    .complete
        \\  .report: termese.TermReport
        \\    .associated_text: termese.AssociatedTextReport
        \\      .key: termese.KeyReport
        \\        .mod: termese.KeyMod
        \\          .shift: bool = false
        \\          .alt: bool = false
        \\          .control: bool = true
        \\          .super: bool = true
        \\          .hyper: bool = false
        \\          .meta: bool = false
        \\          .capslock: bool = false
        \\          .numlock: bool = false
        \\        .value: termese.Key
        \\          .char: u21 = 97
        \\        .event: termese.KeyEvent
        \\          .repeat
        \\        .shifted: u21 = 0
        \\        .base_key: u21 = 0
        \\      .text: []const u8
        \\        "αβγδ"
        \\  .rest: []const u8
        \\    ""
        ,
    ).expectEqual(term.read("\x1b[97;13:2;945:946:947:948u"));
}
