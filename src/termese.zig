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

quirks: Quirks = Quirks{},

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
pub fn read(term: TermRead, in: []const u8) Reply {
    switch (in[0]) {
        // NUL is legacy for Ctrl-space.
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
        0x09 => return Reply.ok(special(.tab)),
        // https://vt100.net/docs/vt100-ug/chapter3.html#T3-5
        0x1c => return Reply.ok(modText(mod().Control(), '\\'), in[1..]),
        0x1d => return Reply.ok(modText(mod().Control(), ']'), in[1..]),
        0x1e => return Reply.ok(modText(mod().Control(), '~'), in[1..]),
        0x1f => return Reply.ok(modText(mod().Control(), '?'), in[1..]),
        0x20 => return Reply.ok(text(' '), in[1..]),
        0x1f => { // <BS>
            if (term.quirks.backspace_as_delete) {
                return Reply.ok(modKey(mod().Control(), specialKey(.backspace)), in[1..]);
            } else {
                return Reply.ok(special(.backspace), in[1..]);
            }
        },
        0x1b => { // The Main Event!
            return term.parseEsc(in);
        },

        else => unreachable, // TODO: not unreachable at all...
    }
}

fn parseEsc(term: TermRead, in: []const u8) Reply {
    if (in.len == 1) {
        return Reply.ok(special(.esc), in);
    }
    switch (in[1]) {
        'O' => return term.parseSS3(in),
        '[' => return term.parseCSI(in),
        0x1b => { // Legacy Alt-Esc
            return Reply.ok(modKey(mod().Alt(), specialKey(.esc)), in[2..]);
        },
        else => { // Lead alt probably.
            const reply = term.read(in[1..]);
            if (reply.status == .complete) {
                switch (reply.report) {
                    .key => |k| {
                        // Lead-alt is legacy, so we only care about modifier,
                        // and key:
                        var kmod = k.mod;
                        return Reply.ok(modKey(kmod.Alt(), k.key));
                    },
                    .paste,
                    .mouse,
                    .info,
                    .more,
                    => {
                        // Plausible interpretation: Esc followed by something
                        // else.
                        return Reply.ok(special(.esc), in[2..]);
                    },
                    .unrecognized, .malformed => return reply,
                }
            }
        },
    }
}

fn parseSS3(term: TermRead, in: []const u8) Reply {
    assert(in[0] == 0x1b);
    assert(in[1] == 'O');
    // If that's all we got, there may well be more coming:
    if (in.len == 2) return moreNeeded(false, in);

    switch (in[2]) {
        'A' => return Reply.ok(special(.up), in[2..]),
        'B' => return Reply.ok(special(.down), in[2..]),
        'C' => return Reply.ok(special(.right), in[2..]),
        'D' => return Reply.ok(special(.left), in[2..]),
        'H' => return Reply.ok(special(.home), in[2..]),
        'F' => return Reply.ok(special(.end), in[2..]),
        'P'...'S' => |fk| {
            return Reply.ok(fKey(fk - 0x4f), in[2..]);
        },
        else => { // Valid esc code, don't know what
            return notRecognized(in[0..2], in[2..]);
        },
    }
    _ = term;
}

fn parseCSI(term: TermRead, in: []const u8) Reply {
    _ = term;
    _ = in;
}

fn parseText(term: TermRead, in: []const u8) Reply {
    const b = std.unicode.utf8ByteSequenceLength(in[0]) catch {
        return malformedRead(1, in);
    };
    const code = std.unicode.utf8decode(in[0..b]) catch {
        return malformedRead(b, in);
    };
    return Reply.ok(text(code), term[b..]);
}

//| Builders.

fn text(char: u21) TermReport {
    return TermReport{ .key = KeyReport{
        .key = Key{
            .char = char,
        },
    } };
}

fn modKey(modifier: KeyMod, key: Key) TermReport {
    return TermReport{ .key = KeyReport{
        .key = key,
        .mod = modifier,
    } };
}

fn modText(modifier: KeyMod, char: u21) TermReport {
    return TermReport{ .key = KeyReport{
        .key = Key{
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
        .key = key_type,
    } };
}

fn specialKey(key_type: KeyTag) KeyReport {
    return KeyReport{ .key = key_type };
}

fn fKey(num: u21) TermReport {
    return TermReport{ .key = KeyReport{
        .key = Key{ .f = @intCast(num) },
    } };
}

fn notRecognized(in: []const u8, rest: []const u8) Reply {
    return Reply{
        .status = .unrecognized,
        .report = TermReport{
            .{ .unrecognized = .{ .sequence = in } },
        },
        .rest = rest,
    };
}

fn moreNeeded(is_paste: bool, rest: []const u8) Reply {
    return Reply{
        .status = .more,
        .report = MoreReport{
            .is_paste = is_paste,
        },
        .rest = rest,
    };
}

fn malformedRead(idx: usize, in: []const u8) Reply {
    return Reply{
        .status = .malformed,
        .report = MalformedReport{
            .sequence = in[0..idx],
        },
        .rest = in[idx..],
    };
}

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
};

/// Status of a read.
pub const ReadStatus = enum(u2) {
    /// We parsed a complete key event.
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
    unrecognized: UnrecognizedReport,
    malformed: MalformedReport,
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

pub const KeyReport = struct {
    mod: KeyMod = KeyMod{},
    key: Key,
    action: KeyAction = .press,
    shifted: u21 = 0,
    base_key: u21 = 0,
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

/// Type of key action.
pub const KeyAction = enum(u2) {
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

    pub fn Shift(m: *KeyMod) KeyMod {
        m.shift = true;
        return m;
    }

    pub fn Alt(m: *KeyMod) KeyMod {
        m.alt = true;
    }

    pub fn Control(m: *KeyMod) KeyMod {
        m.control = true;
        return m;
    }

    pub fn Super(m: *KeyMod) KeyMod {
        m.super = true;
        return m;
    }

    pub fn Hyper(m: *KeyMod) KeyMod {
        m.super = true;
        return m;
    }

    pub fn Meta(m: *KeyMod) KeyMod {
        m.meta = true;
        return m;
    }

    pub fn Capslock(m: *KeyMod) KeyMod {
        m.capslock = true;
        return m;
    }

    pub fn Numlock(m: *KeyMod) KeyMod {
        m.numlock = true;
        return m;
    }
};

/// All possible Key types.
pub const KeyTag = enum(u4) {
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
    null,
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
    null,
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
pub const KeyPadKey = struct {
    code: KeyPadCode,

    /// Return the value of the button pressed.  Some such
    /// values are from the Private Use Area and must be
    /// translated into a normalized form.
    pub fn value(key: KeyPadKey, iso_sep: bool) u21 {
        switch (key.code) {
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
            .KP_Enter => return '\x0d',
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

pub const KeyPadCode = enum(u5) {
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
