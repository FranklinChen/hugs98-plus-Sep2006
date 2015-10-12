This defines an example Html page.

> import Html

<p> We define our page as:</>

> htmlPage :: Html
> htmlPage
> 	= header
>	  << thetitle 
>	     << "My Haskell Home Page"
>    +++ body ! [bgcolor "#aaff88"] << theBody

The definition of htmlPage reads: First we have a header, which
contains a title, which contain the text "My Haskell Home Page". After
this, we have a body, with attribute bgcolor set to #aaff88, and this
body contains the Html defined by theBody. Don't worry about the type
of things right now, just try get a feel for what the combinators look
like.

> theBody :: Html
> theBody =
>       table ! [border 0] << tableContents
>   +++ br
>   +++ p << message
> message 
>  = "Haskell is a general purpose, purely functional programming language."

<p> This reads: the body is a table (with a border), the contents of
the table are defined by <:h>tableContents</>. This is followed by a
br (an explicit line break), and a paragraph containing a message.</>

<p> Now need to define the tableContents. For this we use our special
table combinators.</>

> tableContents :: HtmlTable
> tableContents = (haskell `above` purely) `beside` lambda
>     where
>	haskell = td ! [align "center"]
>	            << font ![size "7",face "Arial Black"] 
>			<< "Haskell"
>	purely  = td << font ! [size "6"] 
>			<< "A Purely Functional Language"
>	lambda  = td << image ! [src "lambda.gif"]

<p>
This produces a table of cells, which nest like this:
</>
<table border="1">
  <tr>
    <td>
	haskell
    </td>
    <td rowspan="2">
	lambda
    </td>
  </tr>
  <tr>
    <td>
	purely
    </td>
  </tr>
</table>

<p>
Even though the lambda box sits over two rows, the
semantics of above and beside handle this correctly.
</>

Now we can render our HTML page.

> main = writeFile "example.htm" (renderHtml htmlPage)
