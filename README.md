# Tiny Life
A toy Game Of Life for the TIC-80

I had a bunch of fun doing this, and also learned quite a lot. It became quite more than I initially intended. It took quite more code than I was expecting.

The code turned out cryptic in some places, because I kept hitting my head on the 64k limit from TIC 0.70.6, as well as having performance problems, so I had to shorten stuff dramatically and also resort to optimizations that added to the complexity of the code. There's also some indentation shenanigans going on in there that one may find strange. This is because my brain (and my mouse wheel) explodes when editing too much code in one file, so I had to find a way to stay organized. I separated the code into sections, indented their content, and that gives me the ability to fold the sections in Sublime (I don't know if that works in other editors).

On the bright side, I learned a lot about how to optimize lua. While the older version used to run at 15-20fps on my end, the new version runs at 45-55fps. 

There are still some things I want to add to it. It's by no means finished. I think there may still be a little room for more optimizations, but mostly there's a few features I didn't get to work on yet (listed in the TODO), which I'd really like to.

I will come back to it as I have time or patience.

