## Command Processing

~[Macro]~

clim:define-command-table :
:   *name **&key** inherit-from menu inherit-menu*

~[Function]~

clim:make-command-table :
:   *name **&key** inherit-from inherit-menu (errorp t)*

By default command tables inherit from `global-command-table`. According
to the `CLIM2.0 specification`, a command table inherits from no command
table if `nil` is passed as an explicit argument to *inherit-from*. In
`revision 2.2` all command tables must inherit from
`global-command-table`. McCLIM treats a `nil` value of *inherit-from* as
specifying `'(global-command-table)`.
