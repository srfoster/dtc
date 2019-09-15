#lang scribble/manual
@require[@for-label[dtc
                    racket/base]]

@title{dtc}
@author{thoughtstem}

@defmodule[dtc/hello/normal]

@defproc[(print [s string?])
         void?]{
  
}

@defmodule[dtc/hello/colors]

@defproc[(print [s string?])
         void?]{
  
}


@defmodule[dtc/hello/animation]


@defproc[(print [s string?])
         void?]{
  
}



@defmodule[dtc/story/images]

No functions.  Just turns Stories into images.


@defmodule[dtc/story/cats]

Fun with cats..


@defproc[(cat) void?]{
  
}

@defproc[(first-viral-cat) void?]{
  
}

@defproc[(edison-cat) void?]{
  
}

@defproc[(authors-cat) void?]{
  
}

TODO: Additional cats....


@defproc[(rotate) void?]{
  
}

@defproc[(shrink) void?]{
  
}

@defproc[(redify) void?]{
  
}

@defproc[(blueify) void?]{
  
}

@defproc[(greenify) void?]{
  
}

@defproc[(orangeify) void?]{
  
}

@defproc[(purpleify) void?]{
  
}

@defproc[(yellowify) void?]{
  
}


TODO: Additional operations


@defmodule[dtc/story+/images]

No functions.  Just produces images, but with a different syntax.


@defmodule[dtc/story+/cats]

All the same as dtc/story/cats.  Diff syntax..

TODO: Document here or there? Depends on how other one shapes up..

Note: Certain racket functions work sqrt.  Reprovide?

@defmodule[dtc/frames/cats]

Has image function.  Has everything else from story/cats

@defproc[(image [s Story?])
         image?]{
  
}

@defproc[(rotate-left) void?]{
  
}

@(require (prefix-in a: dtc/frames/animations))

@defproc[(image [moments (listof moment?)])
         image?]{
  Takes a Story (also known as a @racket[list]) and turns it into an image.

  A simple example would be

  @codeblock{
    #lang dtc/frames/animations
    
    (image '(this is a story))
  }

The list may contain nested Stories, in which case, the sub-Stories are rendered in a nested 
part of the resulting image.

Example:

  @(a:image '(this is a test))

  @(a:image `(this `(is a test)))

  @(a:image `(this `(is `(a test))))
}


@defproc[(animation [moments (listof moment?)])
         image?]{
  
}



@defproc[(napoleon/turk [n number?])
         image?]{
  
}

@defproc[(napoleon/turk-raw [n number?])
         Story?]{
  
}


@defproc[(image-chess [s ChessStory?])
         image?]{
  
}

@defproc[(image-tic-tac-toe [s TicTacToeStory?])
         image?]{
  
}


@defmodule[dtc/complete]

Has stuff from frames -- e.g. image.


@defform[(define word def)]{
  
}


@defform[(define (word slot ...) def)]{
  
}

@defform[(if c t b)]{
  
}

@defform[(cond [if then] ... [else __])]{
  
}

@defproc[(print [s string?])
         void?]{
  
}

@defproc[(~a [s string?] ...)
         image?]{
  
}

@defproc[(eq? [x any/c] [y any/c])
         image?]{
  
}


@defproc[(current-day)
         image?]{
  
}


@defproc[(image-code [s Story?])
         image?]{
  
}

@defproc[(shuffle [s Story?])
         Story?]{
  
}

@defproc[(animate-deck [s Story?])
         image?]{
  
}

@defmodule[dtc/English]

Fake lang.

