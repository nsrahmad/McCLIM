## Using command tables

A *command table* is an object that is used to determine what commands
are available in a particular context and the ways in which commands can
be executed.

Simple applications do not manage command tables explicitly. A default
command table is created as a result of a call to the macro
`define-application-frame` and that command table has the same name as
the application frame.

Each command table has a *name* and that CLIM manages a global
*namespace* for command tables.

~[Function]~

find-command-table :

:	*name **&key** (errorp t)*

	This function returns the command table with the name *name*. If there
    is no command table with that name, then what happens depends on the
    value of *errorp*. If *errorp* is *true*, then an error of type
    `command-table-not-found` is signaled. If *errorp* is *false*, otherwise
    `nil` is returned.

~[Macro]~

define-command-table : 

:    *name **&key** inherit-from menu inherit-menu*

~[Function]~

make-command-table : 

:   *name **&key** inherit-from menu inherit-menu (errorp t)*

    By default command tables inherit from `global-command-table`. According
    to the CLIM 2.0 specification, a command table inherits from no command
    table if `nil` is passed as an explicit argument to *inherit-from*. In
    revision 2.2 all command tables must inherit from
    `global-command-table`. McCLIM treats a `nil` value of *inherit-from* as
    specifying `’(global-command-table)`.
