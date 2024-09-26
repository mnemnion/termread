# Termread

This is a library which does one thing: translate messages from a terminal into usable events.

It does not:

- Set up a terminal
- Operate an event loop
- Read from stdin
- Write to stdout

It does:

- Take a slice, and make a `TermReport` from the contents at `[0]`.  This is an error-free process which always results in a TermReport, which includes the rest of the slice.

The intention is to simplify and make useful an often-gnarly aspect of writing terminal programs, while imposing no opinion or larger framework on the composition of that program.  It's great that there are full frameworks like Vaxis available for rich TUI programming, but if you're on your own journey, and just want something which parses terminal input, that's Termread.

## Quirks

Gone are the days when emulation was a meaningful concept.  Modern terminals speak a common language, with minimal incompatibilities.  Termread understands that language.  Offer applies to Unix terminals only.

There are a few quirks in the various implementations, which Termread may be configured to understand.  More may be added, so long as they aren't fixes for terminal bugs, but actual differences of opinion about what a given sequence is supposed to mean.

This understanding applies to the modified CSI u mode which has recently become popular, as well as CSI u mode itself, and legacy formats.  XTerm `ModifyOtherKeys` is not natively supported, although that could change with sufficient interest (and someone who uses XTerm and can test the result).  The various mouse protocols (except highlight mode), paste bracketing, and basic terminal reports are all supported.  Less basic terminal reports may be added, as time and interest allows.

The intention is to support the useful and _used_ communiqués which a contemporary terminal program will actually use.  It is a non-goal to support every variation on terminals which has ever been emulated, nor the more exotic Xterm-style modes such as Tektronik 4014 or DEC Locator mouse tracking.  Support for private-use terminal replies is minimal: patches for those will be considered.  If there's a commonly-understood sequence you'd like to see added, and I haven't gotten around to it already or just missed it, then if you can describe the use you have for it, I will add it to the collection.

What about Termwrite?  Yeah... maybe.  The domain doesn't generalize as well.

## Use Notes

Termread assumes that the terminal is set to raw mode.  All setup and teardown is the responsibility of the host program.

The library is non-allocating.  A `TermRead` struct contains a small buffer which is adequate to translate terminal reports to a useful form, and this will be overwritten on the next read which needs to make use of it.  Other reporting will have a slice, this is a view into the read buffer.  Should you find it necessary, you can call `try term_report.toOwned(allocator)`, which will duplicate any views and return a copy of the TermReport which owns the memory outright, this is a no-op on many reports, but harmless.  Similarly, `term_report.deinit(allocator)` will dispose of owned memory, if there is any: this is only safe to call on owned term reports, but also resolves to a no-op when there's nothing to deinit.

## Roadmap

- [ ] Add test executable to set modes and print TermReports from stdin.
    - [ ] Complete the set of fmt printers.
    - [ ] Basic executable.
    - [ ] Configurations for different modes.
- [ ] TermString: Plain text event descriptor which parses to TermReports.
      Basically Vim-like syntax, but expanded to cover the weird stuff.
- [ ] Hash and eql functions (HashMap context object).

Maybe:

- [ ] If I figure out a really good data structure for resolving sequences
      of terminal events into some T, I'll include it here.
