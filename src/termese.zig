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

const std = @import("std");
const assert = std.debug.assert;

/// Varieties of Termese understood by termese.zig
pub const DialectKind = enum {
    legacy,
    kitty,
    // Probably need a couple more, windows? xterm proper?
};

/// Basic category of a read event.
pub const TermEventKind = enum(u2) {
    key,
    info,
    paste,
    // More?
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
};
