---
title: Typescript
---

There are a lot of attempts to replace Javascript by anothers languages [[see here]](https://github.com/jashkenas/coffee-script/wiki/List-of-languages-that-compile-to-JS).
Typescript, designed by Microsoft, is one of them.
One of the main reason to create a language that compile to Javascript is to have a static typed language on client side.
Like others, that's Typescript does.

## Why Typescript ?

So, what's make Typescript different from the others ?  
I think people who are behind Typescript understand one important thing about the front-end developpers:
they love Javascript and they don't want it to be replaced by another completely different language.
That why they decided to create something on top of Javascript. More exactly, a superset of Javascript.  
What does it mean ?
It  means that Javascript is Typescript compliant and Typescript brings some new features (static typing) to Javascript.

### How does it work ?
In order to type all the existing Javascript code, Typescript introduce something called "declaration files (*.d.ts)".
A declaration file is a file where you transpose your Javascript code into Typescript definitions of objects, functions, classes and interfaces.
In that way, Typescript is able to perform type checking on Javascript code.
On this [repository](https://github.com/borisyankov/DefinitelyTyped), you can find a lot of declaration files for many libraries.

By default, Typescript use a declaration file named "lib.d.ts".
It's role is to type all the ECMAScript APIs (Window, Object, Function etc...).
I will tell you more about this file in the following paragraph.

## Cool but...
I think there are some problems with the use of theses declarations files.
The Javascript world is huge. One day, you will want to use a Javascript library that don't have any declaration files.
If you want to have type checking on this library, you will have no choice that write a declaration file yourself for *different versions* of the library.
That can be very tedious.

Regarding to the declaration file "lib.d.ts", this one is provided with the Typescript compiler and has the hard mission to define all ECMAScript APIs.
More exactly, it defines the common ECMAScript and specific properties come from Internet Explorer.
So, Typescript compiler will not compile if you use some (new) features from HTML5 that are available in Chrome but not in IE.
I can understand that it would be very hard to maintain a such file that define ECMAScript for each browser and each version of them.  
So, Is it the right way to do ?

Another annoying point is that we don't know where to get a declaration file for a specific version of one Javascript library (like JQuery).
One anwser could be to create a kind of package manager devote to declaration files.

## Typescript in the real world.
Despite of theses drawbacks, I think I found a way to make Typescript more confortable to use.
That's what we are talking about from now !

### Environnement developpment.
Having a cosy environnement developpment is very important to be effective.
As exemple, here how I define my own:

* I don't use Visual Studio.
* I don't care about autocompleting.
* I just code with a text editor (emacs).
* I set up the [Typescript emacs mode]() provided by Microsoft.
* I use grunt combined with [grunt-typescript](https://github.com/k-maru/grunt-typescript), [grunt-contrib-watch](https://github.com/gruntjs/grunt-contrib-watch) and others plugins to build my Typescript base code.

#### Grunt project skeletons
My "[typescript-project-skeletons](https://github.com/srenault/typescript-project-skeletons)" repository presents two kinds of Typescript projects configuration.
The [first one](https://github.com/srenault/typescript-project-skeletons/tree/master/no-server) is a simple static project (HTML, Typescript and CSS).
The [second one](https://github.com/srenault/typescript-project-skeletons/tree/master/with-play) offers one valid way to intergrate Typescript code into a play framework project.

*Note: I aware of some sbt plugins that compile Typescript, but I prefer use some front technology like "grunt" to manage my "front" base code.*

### Clever type checking.
My idea is to make a compromise and use type checking only on the code I write.
In order to do that, there are two two things to do:

* Declare external Javascript libraries as "any" type. Don't use theirs declaration files.
  For instance, to declare "underscore" as "any" type, Typescript offer this syntax:

        declare var _: any;
        _.anything; //No compile error.

* Don't use the default provided "lib.d.ts" declaration file.
  Like external Javascript libraries, declare "window" as "any" type.
  Declare each property of "window" as "any" type.
  To do that, I created a [script]() that generate a declaration file.

The "grunt-typescript" plugin has an "nolib" option.
When having the "true" value, the internal "lib.d.ts" declaration file isn't used.

### Functionnal programming.
Typescript can give more than just typing to Javascript.
My idea is to provide to Typescript developper a *core set of functionnal libraries*.
I already [started](https://github.com/srenault/typescript-fp) to create a functionnal library that offer immutable structure like "List" and "Option".
It's just an inception so, feel free to help me on this task.

## The End.
I think Typescript as an new alternative to create front-end application.
It deserve that we spend time to discover it.
The fact that it is a superset of Javascript is really cool.
In my opinion, It has more chance to be adopted by front guys developper than any other languages that compile to Javascript.
Good luck !
