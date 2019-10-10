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


@section{Complete}

@defmodule[dtc/complete]

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


@(require dtc/complete)

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
}





