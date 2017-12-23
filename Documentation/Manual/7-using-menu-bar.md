## Using Menu bar

Menu bar has become essential part of every GUI system, including
McClim. Ideally, McClim should try to use the menu bar provided by host
window system via McClim backends, but the current `clx-backend` doesn't
supports native menu bars. That's why It has some quirks of its own,
like you need to keep mouse button pressed while accessing the
sub-menus.

### Creating Menu bar

McCLIM makes creating menu bar quite easy.

```commonlisp
(clim:define-application-frame foo ()
...
	(:menu-bar t)
...)
```

Options for `:menu-bar` can be

-   `t` (default) : McClim will provide the menu bar. Later, when you
    start defining commands, you can provide a `(:menu t)` argument to
    command definition that will add this command to menu bar.

-   `nil` : McClim won't provide the menu bar.

-   `command-table` : If you provide a named command table as argument,
    that command table is used to provide the menu bar(See Chapter
    [\[using-command-tables\]](#using-command-tables){reference-type="ref"
    reference="using-command-tables"} on command tables).

To add a sub-menu to menu bar, You need to change the type of menu-item
from `:command` to `:menu` (which requires another `command-table` as
argument) (See ).

### Modifying Menu bar

Menu bar can be changed anytime by changing `command-table` associated
with the current `frame`. For example:

```common-lisp
(setf (frame-command-table *application-frame*) 
	new-command-table)
```

changes menu bar of `*application-frame*` by replacing current
`command-table` (accessible by `frame-command-table` function) with
`new-command-table`.

### Modifying menu items of command table {#modifying-menu-items}

Menu items can be added to command table with following function:

~[Function]~

add-menu-item-to-command-table :

:   *command-table string type value **&key** documentation 
	(after :end) keystroke text-style (errorp t)*

	Arguments to this function are:
	-   `command-table` : Command table to which we want to add the menu
		item.

	-   `string` : name of the menu item as it will appear on the menu bar.
		Its character case is ignored e.g. you may give it `file` or `FILE`
		but it will appear as `File`.

	-   `type` and `value` : type could be one of
		`:command`,`:function`,`:menu` and `:divider`. value of `value`
		depends on `type`. So when given the

			-   `:command` : `value` must be a command, a cons of command name
			and it's arguments. if you omit the arguments McCLIM will prompt
			for arguments.

			-   `:function` : `value` must be a function having indefinite
			extent that, when called, returns a command. Function must
			accept two arguments, the gesture (keyboard or mouse press
			event) and a "numeric argument".

			-   `:menu` : `value` must be another command table. This type is
			used to add sub-menus to the menu.

			-   `:divider` : `value` is ignored, and `string` is used as a
			divider string. Using "\|" as string will make it obvious to
			users that it is a divider.

	-   `documentation` : You can provide the documentation (for non-obvious
		menu items) which will be displayed on pointer-documentation pane
		(if you have one).

	-   `after` (default `:end`) : This determines where item will be
		inserted in the menu. The default is to add it to the end. Other
		values could be `:start`, `:sort`(add in alphabetical order) or
		`string` which is name of existing menu-item to add after it.

	-   `keystroke` : If keystroke is supplied, it will be added to comand
		tables keystroke accelerator table. Value must be a keyboard gesture
		name e.g. (:s :control) for Control + s.

	-   `text-style` : This is either a text style spec or nil. It is used
		to indicate that the command menu item should be drawn with the
		supplied text style in command menus.

	-   `error-p` : If this is `t`, adding an existing item to the menu will
		signal error. If `nil`, it will first remove the existing item and
		add the new item to the command-table.

To remove items from command table, following function is used:

~[Function]~

remove-menu-item-from-command-table :

:   *command-table string **&key** (errorp t)*

	where `command-table` is command-table-designator and `string` is menu
	item's name (it is case-insensitive). You can provide `:error-p nil` to
	suppress the error if item is not in the command-table.

Note that both of above functions `does not` automatically update the
menu bar. For that you need to replace existing `frame-command-table`
with modified command table using `setf`. Ideal way to do this is use
`let` to create the copy of `frame-command-table`, modify it and at the
end call `setf` to replace the original.
