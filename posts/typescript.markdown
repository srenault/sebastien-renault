---
title: Typescript
---

# Why Typescript ?
There are a lot of attempts to replace Javascript by anothers languages [see here](https://github.com/jashkenas/coffee-script/wiki/List-of-languages-that-compile-to-JS).
Typescript, designed by Microsoft, is one of them.
One of the main reason to create a language that compile to Javascript is to have a static typed language on client side.
Like others, that's Typescript does.
What's make Typescript different from the others ?
I think the designers of Typescript understand one important thing about the front-end developpers:
they love Javascript and they don't want it to be replaced by another completely different language.
That why they decided to create something on top of Javascript. More exactly, a superset of Javascript.
What does it mean ?
It  means that Javascript is Typescript compliant and Typescript brings some new features to Javascript.
Obviously, the main feature that Typescript brings to Javascript is the static typing.

# How to live in a world where everything is dynamic ?

## Declaration files.

### Type the world.
In order to type all the existing Javascript libraries and browsers API, Typescript introduce something called "declaration files (*.d.ts)".
A declaration file is a file where you transpose your Javascript code into Typescript definitions (no implementations) of objects, functions, classes and interfaces.
In that way, Typescript is able to perform the type checking on Javascript code.
There is a repository on github where you can find a lot of declaration files for many libraries: [see here](https://github.com/borisyankov/DefinitelyTyped).

By default, Typescript use a declaration file named "lib.d.ts".
It's role is to type all the ECMAScript APIs (Window, Object, Function etc...).

### A good idea but...
I think there are some problems with the use of theses declarations files.
The Javascript world is huge. You can be sure that one day, you will want to use a Javascript library that don't have any declaration files.
If you want to have the type checking on this library, you will have to spend (more or less) time to write and maintain it for *different versions*.

Regarding to the declaration file "lib.d.ts", this file provided by the Typescript core have the hard mission to define all ECMAScript APIs.
It's almost impossible to do this correctly for every versions of each browser without automating tools.
It's for this reason that "lib.d.ts" define only some extra extensions for Internet Explorer.

Another annoying point is that we don't know where to get a declaration file for a specific version of one Javascript library.
One anwser could be to create a kind of package manager devote to declaration files.

## My use of Typescript.
My idea is to perform the type checking only on the code I write.
It's means some part of the code will be type checked and others parts won't.

### My development environnement.
I don't use Visual Studio.
I just code with a text editor (emacs).
I don't care about autocompleting.
I set up the Typescript mode provided by Microsoft. On my github, I push the [code](https://github.com/srenault/typescript-mode) and fix little thing about the boolean keyword.
I use grunt ([grunt-typescript](https://github.com/k-maru/grunt-typescript) combined with [grunt-contrib-watch](https://github.com/gruntjs/grunt-contrib-watch)) to build the Typescript code automaticaly.
[Here](https://github.com/srenault/typescript-project-skeletons) is a repository that show two possible configrations:
    - The first one don't use any web server (just static HTML/CSS/Typescript files).
    - The second is an example of Typescript used inside a [Play Framework project]("https://github.com/playframework/playframework").

### How to not do type checking  ".
As I said previously, this declaration file can't define the ECMAScript APIs for all browsers.
That's why, I decided to not use it.
To do 

### More than static typing ?.
Typescript can give more than juste typing to Javascript.
My idea is to provide to Typescript developper a *core set of functionnal libraries*.
