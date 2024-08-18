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

/// Configure Termese to recognize known quirks in terminal reporting.
/// This is a place for enduring differences of opinion in dialect, not
/// an all-purpose bugfix bucket.
pub const Quirks = packed struct(u8) {
    /// Terminal sends 0x08 for <BS>, not 0x7f
    backspace_as_delete: bool = false,
    /// Reserved for other quirks
    reserved: u7 = 0,
};

/// Basic category of a read event.
pub const TermEventKind = enum(u3) {
    /// A key event: one or several keys pressed.
    key,
    /// A reply from the terminal to some request.
    info,
    /// Pasted data.
    paste,
    /// Mouse movement.
    mouse,
    // A garbled or otherwise invalid sequence.
    malformed,
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

/// All possible Key types.
pub const KeyTag = enum(u4) {
    Backspace,
    Enter,
    Left,
    Right,
    Up,
    Down,
    Home,
    End,
    PageUp,
    PageDown,
    Tab,
    BackTab,
    Delete,
    Insert,
    F,
    Char,
    KpKey,
    Null,
    Esc,
    CapsLock,
    ScrollLock,
    NumLock,
    PrintScreen,
    Pause,
    Menu,
    KeypadBegin,
    Media,
    Modifier,
};

pub const Key = union(KeyTag) {
    Backspace,
    Enter,
    Left,
    Right,
    Up,
    Down,
    Home,
    End,
    PageUp,
    PageDown,
    Tab,
    BackTab,
    Delete,
    Insert,
    F: u6,
    Char: u21,
    KpKey: KeyPad,
    Null,
    Esc,
    CapsLock,
    ScrollLock,
    NumLock,
    PrintScreen,
    Pause,
    Menu,
    Media: MediaKey,
    Modifier: ModifierKey,
};

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

pub const KeyPad = struct {
    code: KeyPadCode,

    pub fn value(key: KeyPad, iso_sep: bool) u21 {
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
