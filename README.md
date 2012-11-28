# Bufshow: A simple presentation tool for Emacs.



Bufshow mode is a presentation tool for Emacs.  Enabling the
`bufshow-mode` global minor mode is the first step to using it.
You'll also need to define an elisp vector that contains the list
of files and tokens to use during the presentation and invoke
`bufshow-load` or `bufshow-start` to start the presentation.

There are key bindings to move to the next and previous slides.
With an Emacs daemon and emacsclient it's easy to invoke the
`bufshow-next` and `bufshow-prev` functions using an IR remote
and something like lirc.

For more information on how to configure a presentation see the
`bufshow-start` function documentation.


# Creating a Presentation



Start by creating an elsip file that contains a call to
`bufshow-start` passing in a vector that represents the slides
and their order. The elements of the SLIDES vector must be lists.
For example:

    (bufshow-start
      [("file1" "token1")
       ("file2" "token2")])

Each list in the vector should contain the following elements in
order:

  1. A string containing the name of a file relative to the
     current directory.  This file will be loaded and
     displayed/narrowed based on the next element.

     Instead of a string this element can be a function, in which
     case the function will be called to show a slide.  Any
     remaining elements in the list will be given to the function
     as arguments.

  2. This element is optional but if present controls how the
     buffer will be narrowed.  The default behavior is to locate
     a line in the buffer that contains `{BEGIN: token}` then
     find a succeeding line that contains `{END}`.  The buffer
     will be narrowed between these lines (exclusive).  Nested
     tokens are not supported.

     Some buffers have special behaviors when you supply a token
     in this element.  For example, for an `org-mode` buffer the
     token should contain the ID of a heading and bufshow will
     narrow to that org sub-tree.

After you write an elisp file that contains a call to the
`bufshow-start` function with a slides vector, use `bufshow-load`
to evaluate the file and correctly set the base directory for the
relative file names.

You can write your own functions for showing a slide as described
in item 1 above.  Interesting functions provided by bufshow
include:

  * `bufshow-split-below` and `bufshow-split-right` for splitting
    the frame and showing two slides at once.

If your function opens temporary buffers or needs to clean up
after itself you can add lambda expressions to be called after
the slide is changed by using `bufshow-add-clean-up-function`.
Make sure you're using lexical binding so the lambda expressions
end up being closures too.

Your function will have to manually handle narrowing.  You can
use the `bufshow-load-file` and `bufshow-show-token` functions to
perform the same loading and narrowing that bufshow does already.

When you are done with the presentation you can call
`bufshow-stop` to restore the window configuration and turn
`bufshow-mode` off.
