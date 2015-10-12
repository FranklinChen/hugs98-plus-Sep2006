<!DOCTYPE style-sheet PUBLIC "-//James Clark//DTD DSSSL Style Sheet//EN" [
<!ENTITY dbstyle PUBLIC "-//Norman Walsh//DOCUMENT DocBook HTML Stylesheet//EN" CDATA DSSSL>
]>
 
<style-sheet>
<style-specification use="docbook">
<style-specification-body>

(define %chapter-autolabel% #t)
(define %section-autolabel% #t)
(define (toc-depth nd) 3)

(define %html40% #t)
(define %generate-book-titlepage% #t)
(define %generate-book-toc% #t)
(define ($generate-chapter-toc$) #f)

(define use-output-dir #t)
(define %output-dir% "users_guide")
(define %use-id-as-filename% #t)
(define %root-filename% "index")
(define %html-ext% ".html")
(define %stylesheet% "hugs-ug.css")
 
</style-specification-body>
</style-specification>
<external-specification id="docbook" document="dbstyle">
</style-sheet>
