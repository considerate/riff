# RIFF for Haskell

[RIFF][2] has been around for a long time and I thought that now would be a
good time to write a library for it in Haskell. This library supports reading most RIFF
files. Example file formats (taken from Wikipedia and elsewhere) that use RIFF as the 
container layer include:

 - WAV (Windows audio)
 - AVI (Windows audiovisual)
 - RMI (Windows "RIFF MIDIfile")
 - CDR (CorelDRAW vector graphics file)
 - ANI (Animated Windows cursors)
 - DLS (Downloadable Sounds)
 - WebP (An image format developed by Google)
 - XMA (Microsoft Xbox 360 console audio format based on WMA Pro)

And there are many more. You can even come up with your own data that can be contained
inside RIFF.

## Building the Code

To build the code:

    $ cabal sandbox init
    $ cabal install

And that is all that there is to it.

## Bundled Executables

I have written a few simple programs just so that I could test out the code that I have written:

    $ cabal run riff-structure # prints out the internal structure of a RIFF file
    $ cabal run riff-convert # lets you convert files to and from RIFF and RIFX formats
    $ cabal run riff-identity # the identity over RIFF files, useful for testing

Feel free to use any of these executables to have a play with the RIFF file format, or better yet,
write some code using this library and let me know about it.

## Example Data

You can find example data all over the internet. Any WAVE file is a RIFF file for example.
Here is a list of example data that you may be able to download:

 - [John Loomis Examples][1]


 [1]: http://www.johnloomis.org/cpe102/asgn/asgn1/riff.zip
 [2]: http://en.wikipedia.org/wiki/Resource_Interchange_File_Format
