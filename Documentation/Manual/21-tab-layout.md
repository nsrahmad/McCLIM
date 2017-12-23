## Tab Layout {#Tab-Layout}

The tab layout is a composite pane arranging its children so that
exactly one child is visible at any time, with a row of buttons allowing
the user to choose between them.

See also the `tabdemo.lisp` example code located under `Examples/` in the
McCLIM distribution. It can be started using `clim-demo:demodemo`.

~[Class]~

clim-tab-layout:tab-layout :
:   Class precedence list:
	`tab-layout, sheet-multiple-child-mixin, basic-pane, sheet-parent-mixin, pane, standard-repainting-mixin, standard-sheet-input-mixin, sheet-transformation-mixin, basic-sheet, sheet, bounding-rectangle, standard-object, slot-object, t`

	The abstract tab layout pane is a composite pane arranging its
	children so that exactly one child is visible at any time, with a row
	of buttons allowing the user to choose between them. Use
	`with-tab-layout` to define a tab layout and its children, or use the
	`:pages` argument to specify its contents when creating it dynamically
	using `make-pane`.
	
~[Class]~

clim-tab-layout:tab-layout-pane :
:   Class precedence list:
	`tab-layout-pane, tab-layout, sheet-multiple-child-mixin, basic-pane, sheet-parent-mixin, pane, standard-repainting-mixin, standard-sheet-input-mixin, sheet-transformation-mixin, basic-sheet, sheet, bounding-rectangle, standard-object, slot-object, t`

	A pure-lisp implementation of the tab-layout, this is the generic
	implementation chosen by the CLX frame manager automatically. Users
	should create panes for type `tab-layout`, not `tab-layout-pane`, so
	that the frame manager can customize the implementation.
	
~[Class]~

clim-tab-layout:tab-page :
:   Class precedence list: `tab-page, standard-object, slot-object, t`

	Instances of `tab-page` represent the pages in a `tab-layout`. For
	each child pane, there is a `tab-page` providing the page's title and
	additional information about the child. Valid initialization arguments
	are `:title`, `:pane` (required) and
	`:presentation-type`,:DRAWING-OPTIONS (optional).

~[Macro]~

clim-tab-layout:with-tab-layout :
:   *default-presentation-type **&rest** initargs **&key** name 
	**&allow-other-keys** **&body** body*
	
	Return a `tab-layout`. Any keyword arguments, including its name, will
	be passed to `make-pane`. Child pages of the `tab-layout` can be
	specified using BODY, using lists of the form (`title` PANE &KEY
	PRESENTATION-TYPE DRAWING-OPTIONS `enabled-callback`).
	`default-presentation-type` will be passed as `:presentation-type` to
	pane creation forms that specify no type themselves.
	
~[Generic Function]~

clim-tab-layout:tab-layout-pages :
:   *tab-layout*

	Return all TAB-PAGEs in this tab layout, in order from left to right.
	Do not modify the resulting list destructively. Use the `setf`
	function of the same name to assign a new list of pages. The `setf`
	function will automatically add tabs for new page objects, remove old
	pages, and reorder the pages to conform to the new list.
	
~[Generic Function]~

clim-tab-layout:tab-page-tab-layout :
:   *tab-page*

	Returns the `tab-layout` this page belongs to.
	
~[Generic Function]~

clim-tab-layout:tab-page-title :
:   *tab-page*

	Return the title displayed in the tab for this `page`. Use the `setf`
	function of the same name to set the title dynamically.
	
~[Generic Function]~

clim-tab-layout:tab-page-pane :
:   *tab-page*

	Return the CLIM pane this page displays. See also `SHEET-TO-PAGE`, the
	reverse operation.
	
~[Generic Function]~

clim-tab-layout:tab-page-presentation-type :
:   *tab-page*

	Return the type of the presentation used when this page's header gets
	clicked. Use the `setf` function of the same name to set the
	presentation type dynamically. The default is `tab-page`.
	
~[Generic Function]~

clim-tab-layout:tab-page-drawing-options :
:   *tab-page*

	Return the drawing options of this page's header. Use the `setf`
	function of the same name to set the drawing options dynamically.
	
Note: 
:   Not all implementations of the tab layout will understand all
	drawing options. In particular, the Gtkairo backends understands only
	the :INK option at this time.
	
~[Function]~

clim-tab-layout:add-page :
:   *page tab-layout **&optional** enable*

	Add `page` at the left side of `tab-layout`. When `enable` is true,
	move focus to the new page. This function is a convenience wrapper;
	you can also push page objects directly into `tab-layout-pages` and
	enable them using `(setf TAB-LAYOUT-ENABLED-PAGE)`.

~[Function]~

clim-tab-layout:remove-page :
:   *page*

	Remove `page` from its tab layout. This is a convenience wrapper
	around `SHEET-DISOWN-CHILD`, which can also be used directly to remove
	the page's pane with the same effect.
	
~[Generic Function]~

clim-tab-layout:tab-layout-enabled-page :
:   *tab-layout*

	The currently visible tab page of this tab-layout, or NIL if the tab
	layout does not have any pages currently. Use the `setf` function of
	the name to change focus to another tab page.
	
~[Function]~

clim-tab-layout:sheet-to-page :
:   *sheet*

	For a `sheet` that is a child of a tab layout, return the page
	corresponding to this sheet. See also `tab-page-pane`, the reverse
	operation.
	
~[Function]~

clim-tab-layout:find-tab-page-named :
:   *name tab-layout*

    Find the tab page with the specified `title` in `tab-layout`. Note
	that uniqueness of titles is not enforced; the first page found will
	be returned.
	
~[Function]~

clim-tab-layout:switch-to-page :
:   *page*

	Move the focus in page's tab layout to this page. This function is a
	one-argument convenience version of `(setf TAB-LAYOUT-ENABLED-PAGE)`,
	which can also be called directly.
	
~[Function]~

clim-tab-layout:remove-page-named :
:   *name tab-layout*

	Remove the tab page with the specified `title` from `tab-layout`. Note
	that uniqueness of titles is not enforced; the first page found will
	be removed. This is a convenience wrapper, you can also use
	FIND-TAB-PAGE-NAMED to find and the remove a page yourself.
	
~[Generic Function]~

clim-tab-layout:note-tab-page-changed :
:   *layout page*

	This internal function is called by the `setf` methods for
	`tab-page-title` and -DRAWING-OPTIONS to inform the page's tab-layout
	about the changes, allowing it to update its display. Only called by
	the `tab-layout` implementation and specialized by its subclasses.

	


