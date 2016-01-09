# A wiki page -- and a slide show too, by me ?

Just export this page as Slidy (in the sidebar).
Or click to view it as a [Slidy](slideshowdemo?export&format=Slidy),
[DZSlides](slideshowdemo?export&format=DZSlides) slide show now. 
However, S5 slides do not work:
[S5](slideshowdemo?export&format=S5).

# Second slide: A list

- one
- two
- buckle my shoe
- Ladies and Gentlemen

# Third slide: An image
sdf
What do we think about French cheeses?

![](fromage1.jpg)

# Fourth slide: some (poignant) ruby

~~~~{.ruby}
 require 'wordlist'

 # Get evil idea and swap in code words
 print "Enter your new idea: " 
 idea = gets
 code_words.each do |real, code| 
   idea.gsub!( real, code )
 end

 # Save the jibberish to a new file
 print "File encoded.  Please enter a name for this idea: " 
 idea_name = gets.strip
 File::open( "idea-" + idea_name + ".txt", "w" ) do |f|
   f << idea
 end

~~~~

# Fifth slide: No, Learn You a Haskell 
  
~~~~{.haskell}
import System.IO
main = do
    handle <- openFile "girlfriend.txt" ReadMode
    contents <- hGetContents handle
    putStr contents
    hClose handle
~~~~

Running it, we get the expected result:

~~~~{.haskell}
$ runhaskell girlfriend.hs  
Hey! Hey! You! You!  
I don't like your girlfriend!  
No way! No way!  
I think you need a new one!  
~~~~

# Seventh Slide ... for Great Good
~~~~{.haskell}  
-- Let's use a fold to implement searching a list for a sublist.              
search :: (Eq a) => [a] -> [a] -> Bool  
search needle haystack = foldl length_test False (tails haystack)
  where length_test bool x = 
          if take needle_length x == needle then True else bool
        needle_length = length needle 

declaration :: String
declaration = "We hold these truths to be self-evident, " ++
              "that all men are created equal"          
~~~~

Here we get the result:

~~~~{.haskell}
ghci> search "elf" declaration
True
ghci> search "eat" declaration
True
~~~~~

# Eighth Slide: some math?

With some inline: $a = \frac{1}{\pi}$, and some displayed:

$$s = \sqrt{\frac{a}{t^{e^\pi}}}$$

Doesn't work yet in the S5 slide show, only plain webpages -- need to add a link to jsMath!



# Credits

Thanks for random snippets to

[Why's (poignant) guide to ruby](http://poignantguide.net/)

and

[Learn You a Haskell for Great Good!](http://learnyouahaskell.com/)
