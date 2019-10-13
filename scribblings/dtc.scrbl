#lang scribble/manual
@require[@for-label[dtc
                    racket/base]]

@title{dtc}
@author{Stephen R. Foster and Lindsey Handley}

@(require (prefix-in a: dtc/frames/story-images/main))
@(require dtc/story/cats)
@(require (prefix-in h: 2htdp/image))

@(h:scale 2 (dtc-cover))

This is the documentation page for the programming languages used in the book @emph{Don't Teach Coding}.  

@(define (datum->downstory l)
    (define (~i d)
     (h:overlay
      (h:square 120 'solid 'transparent)
      (if (h:image? d)
       d
       (h:text (~a d) 24 'darkgray))))

   (define (rot i) (h:rotate 90 i))

   ((compose rot rot rot)
    (a:image
      (map (compose rot ~i) l)))
)

@(define (center-text s)
  (define ss (string-split s))
  (apply h:above
         (map (curryr h:text 24 'darkgray) ss)))

@(define (p1 t)
  (h:above
   (h:overlay (h:text t 24 'black)
              (h:rectangle 155 50 'solid 'transparent))
   (datum->downstory
     `(,(center-text"A\nWizard's\nTale")
       ,(h:overlay (center-text "A\nLanguage\nWithout")
           (h:square 120 'solid 'darkgreen))
       ,(h:overlay (center-text "A\nLanguage\nWithin")
           (h:square 120 'solid 'darkgreen))
       ,(center-text "Languages\nWithout")
       ,(center-text "Languages\nWithin")))
   (h:square 10 'solid 'transparent)))

@(require (only-in pict inset))

These languages are used in various sections of the book, shown in green below:

@centered{
  @(h:scale 0.58
      (a:image
       (list (p1 "Prologues")
        (p1 "Beginnings")
        (p1 "Middles")
        (p1 "Ends"))))
}

@section{Chapter 1: Prologues}

Here we document the various @racket["Hello, World!"] languages from @bold{Chapter 1}.

@defmodule[dtc/hello/normal #:lang]

@defproc[(print [s string?])
         void?]{
  This simply prints to the screen whatever you give it, allowing you to participate in the age-old tradition of writing @racket["Hello, World!"] programs in the earliest moments of your coding journey.

  Simply:

  @codeblock{
    #lang dtc/hello/normal
    (print "Hello, World!")
  } 

  The output of Stories like @racket[(print "Hello, World!")] are intended to be compared with identical-looking stories written in different languages (see below).  The point is to show by demonstration that syntax can remain constant while semantics differ from language to language.
}

@defmodule[dtc/hello/colors #:lang]

@(require (prefix-in colors: dtc/hello/colors))

@defproc[(print [s string?])
         void?]{
  This allows you to particplate in the age-old tradition of writing @racket["Hello, World!"] programs -- but with a colorful twist!  (I mean, come on, it's the 21st century, @racket["Hello, World!"] programs are allowed to be cooler than just displaying text.)

  Simply:

  @codeblock{
    #lang dtc/hello/colors
    (print "Hello, World!")
  } 

  Gives you something a bit different every time:

  @(random-seed 42)

  @(colors:print "Hello, World")
  @(colors:print "Hello, World")
  @(colors:print "Hello, World")

}


@defmodule[dtc/hello/animation #:lang]



@defproc[(print [s string?])
         void?]{
  This allows you to particplate in the age-old tradition of writing @racket["Hello, World!"] programs -- but with an @emph{animated} twist!  

  @codeblock{
    #lang dtc/hello/colors
    (print "Hello, World!")
  } 

That produces an animation describable with this Story:

@(let ([hw
         (h:overlay
           (h:text "Hello, World" 24 'red)
           (h:circle 50 'solid 'white))]
       [s (h:square 200 'solid 'black)])
   (a:image
     (list
      (h:overlay (h:rotate 0 hw) s)
      (h:overlay (h:rotate -45 hw) s)
      (h:overlay (h:rotate -90 hw) s))))



}

@section{Chapter 2: Beginnings}

Here we document the various cat-related and Story-related languages from @bold{Chapter 2}. 

@defmodule[dtc/story/images #:lang]

This language has no specific vocabulary.  In fact, @emph{any} Story is valid.

@bold{Rules}: Moments should be separated by an arrow (@racket[->]).  Separate Stories must go on separate lines.

This language simply compiles valid Stories to images.  For example, this program will produce three images:

@codeblock{
  #lang dtc/story/images
  
  today -> I -> wrote -> "Hello, World"
  love -> conquers -> all
  history -> matters

}

Images can be saved to disk within DrRacket by right-clicking on them and selecting @tt{Save image...}

Images can be embedded @emph{in} a Story by placing the cursor where you want the image and clicking @tt{Insert > Insert Image ...} 

@bold{Exercise} Can you produce this image?

@(a:image `(,(h:circle 30 'solid 'red) is a circle))


@defmodule[dtc/story/cats #:lang]

A language with various vocabulary for producing and manipulating images of cats.

@defproc[(cat) void?]{
  A photograph of a kitten from the 1800s, by Harry Pointer -- the Father of Cat Photos and definer of the cat photography genera.
     
  @(h:scale 2 (cat))

}

@defproc[(meta-cat) void?]{
  A photograph of a cat from 1870, by Harry Pointer.  Also, perhaps the first meta-photo of a cat in human history.  Titled @emph{The Photographer}, the image shows a cat apparently operating a camera, while being photographed by a camera.
     
  @(h:scale 2 (meta-cat))
}

@defproc[(first-viral-cat) void?]{
  A frame from one of the first viral cat videos in the history of YouTube.  Uploaded in the year 2006, it can be found @hyperlink["https://www.youtube.com/watch?v=7bcV-TL9mho"]{here}.

  @(h:scale 2 (first-viral-cat))
}

@defproc[(edison-cat) void?]{
  The boxing cats filmed in 1894 by Thomas Edison.  The video can be found @hyperlink["https://www.youtube.com/watch?v=k52pLvVmmkU"]{here}.
 
  @(h:scale 2 (edison-cat))
}

@defproc[(authors-cat) void?]{
  A cat owned by one of the authors.
  
  @(h:scale 2 (authors-cat))
}



@defproc[(rotate) void?]{

  Rotates an image (of a cat, or otherwise):

  @codeblock{
   #lang dtc/story/cats

   cat -> rotate
  }

  Gives:

  @(rotate (cat))

  In other languages, the same can be accomplished with a different syntax: 
  
  @codeblock{
   #lang dtc/story+/cats

   (rotate (cat))
  }
}

@defproc[(shrink) void?]{
  Halves the size an image (of a cat or otherwise).

  @codeblock{
   #lang dtc/story/cats

   cat 
  }

  Gives:
  
  @(cat)

  @codeblock{
   #lang dtc/story/cats

   cat -> shrink
  }

  Gives:
  
  @(shrink (cat))


  @codeblock{
   #lang dtc/story+/cats

   (shrink (cat))
  }
  
  Is the same. 
}

@defproc[(grow) void?]{
  Doubles the size of an image.

  @codeblock{
   #lang dtc/story/cats

   cat 
  }

  Gives

  @(cat)

  @codeblock{
   #lang dtc/story/cats

   cat -> grow
  }

  Gives:

  @(grow (cat))

  
}


@defproc[(rotate-left) void?]{
  Same as @racket[rotate], but in the other direction.

  @(rotate-left (cat))
}

@defproc[(redify) void?]{
  Tints an image red -- e.g.:
  
  @(redify (cat))
}

@defproc[(blueify) void?]{
  Tints an image blue -- e.g.:

  @(blueify (cat))
}

@defproc[(greenify) void?]{
  Tints an image green -- e.g.:

  @(greenify (cat))
}

@defproc[(orangeify) void?]{
  Tints an image orange -- e.g.:

  @(orangeify (cat))
}

@defproc[(purpleify) void?]{
  Tints an image purple -- e.g.:

  @(purpleify (cat))
}

@defproc[(yellowify) void?]{
  Tints an image yellowify -- e.g.:

  @(yellowify (cat))
}

@defproc[(meme-teacher) void?]{
  A famous image used as the basis for the "Unhelpful High School Teacher" meme, reaching peak popularity in 2011.  For more details, see @hyperlink["https://knowyourmeme.com/memes/unhelpful-high-school-teacher"]{knowyourmeme.com}

  @(meme-teacher)
}

@defproc[(dijkstra) void?]{
  Edsger W. Dijkstra (1930-2002), a famous and influential computer scientist -- a prolific writer, researcher, and mathematician.  
  
  @(h:scale 2 (dijkstra))
}

@defproc[(habermann) void?]{
  Nico Habermann (1932-1993), influential computer scientist and student of Dijkstra.  His Wikipedia page can be found @hyperlink["https://en.wikipedia.org/wiki/Nico_Habermann"]{here}.  And his teacher/student family tree (from 1990) can be found @hyperlink["http://taoxie.cs.illinois.edu/nicobig.htm"]{here}.
  
  @(h:scale 2 (habermann))
}

@defproc[(notkin) void?]{
  David Notkin (1955-2013), influential computer scientist and student of Nico Habermann.  His 2013 IEEE Memorandum can be found @hyperlink["https://ieeexplore.ieee.org/document/6519247"]{here}. 

  @(h:scale 2 (notkin))
}

@defproc[(griswold) void?]{
  Influential computer scientist, student of David Notkin.  His Wikipedia page can be found @hyperlink["https://en.wikipedia.org/wiki/Bill_Griswold"]{here}.

  @(h:scale 2 (griswold))
}

@subsection{Bonus images}

Some bonus images of Mesopotamian symbols.  Higher numbers indicate later versions of that symbol.  They can be treated like any other image.  Example:

@codeblock{
  #lang dtc/complete
  
  (image
    `(,(redify (meso-star-1)) 
      ,(orangeify (meso-star-2))
      ,(yellowify (meso-star-3))
      ,(greenify  (meso-star-4))))
}

Produces the Story image of how the symbol for "star" grew more abstract over time:

@(a:image
    `(,(redify (meso-star-1)) 
      ,(orangeify (meso-star-2))
      ,(yellowify (meso-star-3))
      ,(greenify  (meso-star-4))))

@defproc[(meso-star-1) void?]{
  @(meso-star-1)
}

@defproc[(meso-star-2) void?]{
  @(meso-star-2)
}

@defproc[(meso-star-3) void?]{
  @(meso-star-3)
}

@defproc[(meso-star-4) void?]{
  @(meso-star-4)
}

@defproc[(meso-sun-1) void?]{
  @(meso-sun-1)
}

@defproc[(meso-sun-2) void?]{
  @(meso-sun-2)
}

@defproc[(meso-sun-3) void?]{
  @(meso-sun-3)
}

@defproc[(meso-sun-4) void?]{
  @(meso-sun-4)
}

@defproc[(meso-rain-1) void?]{
  @(meso-rain-1)
}

@defproc[(meso-rain-2) void?]{
  @(meso-rain-2)
}

@defproc[(meso-rain-3) void?]{
  @(meso-rain-3)
}

@defproc[(meso-rain-4) void?]{
  @(meso-rain-4)
}


@defproc[(meso-fish-1) void?]{
  @(meso-fish-1)
}

@defproc[(meso-fish-2) void?]{
  @(meso-fish-2)
}

@defproc[(meso-fish-3) void?]{
  @(meso-fish-3)
}

@defproc[(meso-fish-4) void?]{
  @(meso-fish-4)
}

@section{Chapter 3: Middles}

Here we document the various nestable Story languages from @bold{Chapter 3}. 

@defmodule[dtc/story+/images #:lang]

No functions.  Just produces images, but with a different syntax.


@defmodule[dtc/story+/cats #:lang]

All the same as dtc/story/cats.  Diff syntax..

TODO: Document here or there? Depends on how other one shapes up..

Note: Certain racket functions work sqrt.  Reprovide?

@defmodule[dtc/frames/cats #:lang]

Has image function.  Has everything else from story/cats

@defproc[(image [s Story?])
         image?]{
  
}


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
  
  TODO:
  Not all of these are finished...
}

@defproc[(napoleon/turk-raw [n number?])
         Story?]{
  
}


@defproc[(image-chess [s ChessStory?])
         image?]{

  
}

@defproc[(image-tic-tac-toe [s TicTacToeStory?])
         image?]{
  TODO: IMplement 
}


@section{Chapter 4: Ends}

Here we document the various nestable Story languages from @bold{Chapter 4}. 

@defmodule[dtc/complete #:lang]

This language has everthing in it that Racket does, and more.   

We won't document everything from Racket here -- only the vocabulary that's directly used in @emph{Don't Teach Coding}.    

@defform[(define word def)]{
  Allows you to add the new @racket[word] to your language from that point forward.    

  @codeblock{
    #lang dtc/complete
    (define hello "Hello, everybody!")
  }

  From that point forward @racket[hello] can be used without errors.  It's meaning has been given.

}

@defform[(define (word slot ...) def)]{
  Allows you to add a complex expression to your language from that point forward.

  @codeblock{
    #lang dtc/complete
    (define (hello ___) 
       (~a "Hello, " ___ "!"))
  }

  From that point forward @racket[hello] can be used, as long as it matches the pattern @racket[(hello __)].  For example, @racket[(hello "World")] or @racket[(hello (hello "World"))].

}

@defform[(if condition true-branch false-branch)]{
  Is equivalent to @racket[true-branch] when @racket[condition] is @racket[#t] (true), and to @racket[false-branch] otherwise.

  For example:

  @codeblock{
    #lang dtc/complete

    (if (eq? 'tuesday (current-day))
      (print "Cook tacos")
      (print "Cook the usual"))
  } 

  This program will behave differently on Tuesdays because the @racket[(if ...)] expression behaves differently on Tuesdays.
}

@defform[(cond [if then] ... [else __])]{
  Like @racket[if], but supports more two or more branches.

  
  @codeblock{
    #lang dtc/complete

    (cond
      [(eq? 'monday (current-day))
       (print "Cook burgers")]
      [(eq? 'tuesday (current-day))
       (print "Cook tacos")]
      [else (print "Cook the usual")])
  } 

  Note that there are many ways to do conditional branching.  Racket for example, has a construct called @racket[match] which can express the above idea in an even more syntactically clear way

  
  @codeblock{
    #lang dtc/complete

    (match (current-day)
      ['monday  (print "Cook burgers")]
      ['tuesday (print "Cook tacos")]
      [else     (print "Cook the usual")])
  } 

  We mention it here to remind you that there's an entire world of linguistic discovery to be had beyond the pages of these docs...
}

@defproc[(print [s string?])
         void?]{
  Just displays the given string in the interactions window. 
}

@defproc[(~a [s string?] ...)
         image?]{
  Combines a bunch of things into one string.  It converts non-string things (like numbers) into strings.

  @codeblock{
    #lang dtc/complete

    (define number-of-people 5) 
    (define number-of-plates (* 2 number-of-people)) 
    (define number-of-cups   (* 3 number-of-people)) 
    (define bags-of-chips    (/ number-of-people 3)) 
    (define bottles-of-soda  (/ number-of-people 4)) 

    (print
      (~a "Buy:\n" 
          number-of-plates " plates \n"
          number-of-cups " cups \n"
          bottles-of-soda " bottles \n"
          bags-of-chips " chips \n")) 
  }
}

@defproc[(eq? [x any/c] [y any/c])
         image?]{
  Checks if two things are the same.  This is commonly used in conjunction with @racket[if] or @racket[cond]. 

  Something that is the same today might not be the same tomorrow.
}

We'll now document the things that @racket[dtc/complete] adds to Racket.

@defproc[(current-day)
         image?]{
  Depending on the day, returns either @racket['monday], @racket['tuesday], etc. 

  @codeblock{
    #lang dtc/complete

    (define number-of-people 
      (cond 
        [(eq? 'monday (current-day))  5]
        [(eq? 'tuesday (current-day)) 9]
        [(eq? 'thursday (current-day)) 12]
        [(eq? 'friday (current-day))  17]
        [else 0]))

    (if (> number-of-people 10)
      (print "Bring extra chairs")
      (print "Bring the usual stuff"))
  }
}


@(require dtc/frames/image-code)
@(require (only-in dtc/story/cats rotate))

@defproc[(image-code [s Story?])
         image?]{
  Takes a Story and returns an image of it -- so that it can, for example, be embedded in flashcards or other materials that wish to talk about code.  Code is visual; images are a key component of visual storytelling.  If you want to communicate about code, you'll need to be able to make images of it.  Taking a screenshot is the non-automated form of this.

  @codeblock{
    #lang dtc/complete
    
    (image-code 
     `(animate 
        `(cat ,(cat))))
  }

This gives you:


@(image-code 
   `(animate 
      `(cat ,(cat))))

Remember that once something is an image, other vocabulary that pertains to images becomes relevant:

  @codeblock{
    #lang dtc/complete
    
    (rotate
     (image-code 
      `(animate 
        `(cat ,(cat)))))
  }

@(rotate
   (image-code 
     `(animate 
       `(cat ,(cat)))))

}

@defproc[(shuffle [s Story?])
         Story?]{
  
  Takes a story and jumbles its moments up randomly.  

  @codeblock{
    #lang dtc/complete
    
    (define class-members '(alice bob jimmy susan marco ))

    (define random-person
      (first (shuffle class-members)))

    (print
       (~a "The winner is..." random-person))
  }
  
  This program will pick a random class member.
}

@defproc[(animate-deck [s Story?])
         image?]{
  Takes a Story and assumes that each Moment is a Story with two Moments.   This makes the outer Story like a "deck" and the inner Stories like "cards".  Each "card" has a front and a back -- its two Moments.

  The @racket[animate-deck] word displays each front until you press a key.  Then it will display the back.  Then it will go to the next card.  And so on, until all the cards are shown, at which point it loops back to the beginning.


 Here's a program for animating a small deck of two flashcards.

   @codeblock{
    #lang dtc/complete
    
    (animate-deck
     `((cat             ,(cat))
       (first-viral-cat ,(first-viral-cat))))
  }


  TODO: Embed the large programs from the book...
}





