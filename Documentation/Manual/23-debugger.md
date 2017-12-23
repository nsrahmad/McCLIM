# Applications {#Applications}

## Debugger {#Debugger}

The debugger is used for interactively inspecting stack frame when the
unhandled conditions are signalled. Given high enough debug settings it
lets you inspecting frame local variables, evaluating code in it,
examining backtrace and choosing available restarts.

### Debugger usage {#Debugger-usage}

To get up and running quickly with Debugger:

1.  With Quicklisp loaded, invoke in repl:

```common-lisp
(ql:quickload 'clim-debugger)
```

2.  Run simple test condition:

```commonlisp
(clim-debugger:with-debugger (error "test"))
```

Debugger is highly inspired by Slime and uses Swank to gain portability
across implementations. Module is still under development and some
details may change in the future.

Selecting frame with a pointer switches its details and marks it active.
Each locale value may be inspected by selection with mouse pointer.
Active frame is distinguished from others with red color.
`Eval in frame` command evaluates expression in the active frame.

### Keyboard shortcuts {#Keyboard-shortcuts}

Warning: these key accelerators may change in the future.

`M-m`

:   Show more frames

`M-p`

:   Mark previous frame active

`M-n`

:   Mark next frame active

`M-e`

:   Eval in active frame

`TAB`

:   Toggle active frame details

`M-[0-9]`

:   Invoke nth restart

`M-q`

:   Quit debugger

### Debugger API {#Debugger-API}

~[Function]~

debugger :
:   *condition me-or-my-encapsulation*

	Starts debugger with supplied condition. Second argument should be
	supplied by an underlying implementation allowing to encapsulate or
	supply different debugger for recursive debugger calls.
	
~[Macro]~

with-debugger :
:   ***&body** body*

	Wraps the code in `body` to invoke clim debugger when condition is
	signalled (binds `*debugger-hook*` to `#'debugger`).
