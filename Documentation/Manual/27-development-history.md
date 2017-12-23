## Development History

Mike McDonald started developing McCLIM in 1998. His initial objective
was to be able to run the famous "address book" demo, and to distribute
the first version when this demo ran. With this in mind, he worked
"horizontally", i.e., writing enough of the code for many of the
chapters of the specification to be able to run the address book
example. In particular, Mike wrote the code for chapters 15 (Extended
Stream Output), 16 (Output Recording), and 28 (Application Frames), as
well as the code for interactor panes. At the end of 1999, Mike got too
busy with other projects, and nothing really moved.

Also in 1998, Gilbert Baumann started working "vertically", writing a
mostly-complete implementation of the chapters 3 (Regions) and 5 (Affine
Transformations). At the end of 1999, he realized that he was not going
to be able to finish the project by himself. He therfore posted his code
to the free-CLIM mailing list. Gilbert's code was distributed according
to the GNU Lesser General Public Licence (LGPL).

Robert Strandh picked up the project in 2000, starting from Gilbert's
code and writing large parts of chapters 7 (Properties of Sheets) and 8
(Sheet Protocols) as well as parts of chapters 9 (Ports, Grafts, and
Mirrored Sheets), 10 (Drawing Options), 11 (Text Styles), 12 (Graphics),
and 13 (Drawing in Color).

In early 2000, Robert got in touch with Mike and eventually convinced
him to distribute his code, also according to the LGPL. This was a major
turning point for the project, as the code base was now sufficiently
large that a number of small demos were actually running. Robert then
spent a few months merging his code into that produced by Mike.

Arthur Lemmens wrote the initial version of the code for the gadgets in
june of 2000.

Bordeaux students Iban Hatchondo and Julien Boninfante were hired by
Robert for a 3-month summer project during the summer of 2000. Their
objective was to get most of the pane protocols written (in particular
space composition and space allocation) as well as some of the gadgets
not already written by Arthur, in particular push buttons. The
calculator demo was written to show the capabilities of their code.

In July of 2000, Robert invited Gilbert to the LSM-2000 metting in
Bordeaux (libre software meeting). This meeting is a gathering of
developers of free software with the purpose of discussing strategy,
planning future projects, starting new ones, and working on existing
ones. The main result of this meeting was that Gilbert managed to merge
his code for regions and transformations into the main code base written
by Mike, Robert, Iban, and Julien. This was also a major step towards a
final system. We now had one common code base, with a near-complete
implementation of regions, transformations, sheet protocols, ports,
grafts, graphics, mediums, panes, and gadgets.

Meanwhile, Mike was again able to work on the project, and during 2000
added much of the missing code for handling text interaction and
scrolling. In particular, output recording could now be used to
redisplay the contents of an interactor pane. Mike and Robert also
worked together to make sure the manipulation of sheet transformations
and sheet regions as part of scrolling and space-allocation respected
the specification.

Robert had initially planned for Iban and Julien to work on McCLIM for
their fifth-year student project starting late 2000 and continuing until
end of march 2001. For reasons beyond his control, however, he was
forced to suggest a different project. Thus, Iban and Julien, together
with two other students, were assigned to work on Gsharp, an interactive
score editor. Gsharp was the original reason for Robert to start working
on CLIM as he needed a toolkit for writing a graphical user interface
for Ghsarp. The lack of a freely-available version of a widely-accepted
toolkit such as CLIM made him decide to give it a shot. Robert's idea
was to define the student project so that a maximum of code could be
written as part of McCLIM. The result was a complete rewrite of the
space-allocation and space-composition protocols, and many minor code
snippets.

As part of the Gsharp project, Robert wrote the code for menu bars and
for a large part of chapter 27 (Command Processing).

Julien was hired for six months (April to September of 2001) by Robert
to make major progress on McCLIM. Julien's first task was to create a
large demo that showed many of the existing features of McCLIM (a
"killer app"). It was decided to use Gsharp since Julien was already
familiar with the application and since it was a sufficiently
complicated application that most of the features would be tested. An
additional advantage of a large application was to serve as a "smoke
test" to run whenever substantial modifications to the code base had
been made. As part of the Gsharp project, Julien first worked on adding
the possibility of using images as button labels.

Early 2001, Robert had already written the beginning of a library for
manipulating 2-dimensional images as part of McCLIM. A group of four
fourth-year students (Gregory Bossard, Michel Cabot, Cyrille Dindart,
Lionel Vergé) at the university of Bordeaux was assigned the task of
writing efficient code for displaying such images subject to arbitrary
affine transformations. This code would be the base for drawing all
kinds of images such as icons and button labels, but also for an
application for manipulating document images. The project lasted from
January to May of 2001.

Another group of four fourth-year students (Loïc Lacomme, Nicolas Louis,
Arnaud Rouanet, Lionel Salabartan) at the university of Bordeaux was
assigned the task of writing a file-selector gadget presented as a tree
of directories and files, and with the ability to open and close
directories, to select files, etc. The project lasted from January to
May of 2001.

One student in particular, Arnaud Rouanet started becoming interested in
the rest of CLIM as well. During early 2001, he fixed several bugs and
also added new code, in particular in the code for regions, graphics,
and clx-mediums.

Arnaud and Lionel were hired by Robert for the summer of 2001 to work on
several things. In particular, they worked on getting output recording
to work and wrote CLIM-fig, a demo that shows how output recording is
used. They also worked on various sheet protocols, and wrote the first
version of the PostScript backend.

Alexey Dejneka joined the project in the summer of 2001. He wrote the
code for table formatting, bordered output and continued to develop the
PostScript output facility.

In the fall of 2001 Tim Moore became interested in the presentation type
system. He implemented presentation type definition and presentation
method dispatch. Wanting to see that work do something useful, he went
on to implement present and accept methods, extended input streams,
encapsulating streams, and the beginnings of input editing streams. In
the spring of 2002 he wrote the core of Goatee, an Emacs-like editor.
This is used to implement CLIM input editing.

Brian Spilsbury became involved towards the beginning of 2001. His
motivation for getting involved was in order to have
internationalization support. He quickly realized that the first step
was to make SBCL and CMUCL support Unicode. He therefore worked to make
that happen. So far (summer 2001) he has contributed a number of
cosmetic fixes to McCLIM and also worked on a GTK-like gadget set. He
finally started work to get the OpenGL backend operational.
