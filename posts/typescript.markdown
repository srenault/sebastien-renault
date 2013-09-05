---
title: Typescript
---

There are a lot of attempts to replace Javascript by another language [[see here]](https://github.com/jashkenas/coffee-script/wiki/List-of-languages-that-compile-to-JS).
One of the main reason to create a language that compile to Javascript is to bring static typing on client side.
In 2012, Microsoft released a new language named Typescript that does this work.

## Why Typescript ?

So, what's make Typescript different from the others ?  
I think people who are behind Typescript understand one important thing about the front-end developpers:
they love Javascript and they don't want it to be replaced by another completely different language.
That why they decided to create something on top of Javascript. More exactly, a superset of Javascript.  
What does it mean ?
It  means that Javascript is Typescript compliant and Typescript brings some new features (static typing) to Javascript.

### How does it work ?
In order to type all the existing Javascript code, Typescript introduce something called "declaration files" (*.d.ts).
A declaration file is a file where you transpose your Javascript code into Typescript definitions.
This means there are no implementation, just definitions of Javascript code into Typescript modules, functions, objects and interfaces.
In that way, Typescript is able to perform type checking on Javascript code.
There already are many declaration files that have been written for many popular Javascript libraries.
You can find a lot of them on this [repository](https://github.com/borisyankov/DefinitelyTyped).
I invite you to to look at one of theses file.  
In order to use a declaration file inside your project, you have to use this syntax:

    ///<reference path="./underscore.d.ts"/>

Something else you have to know about declaration files is that Typescript use by default a declaration file named "lib.d.ts".
It's role is to type all the ECMAScript APIs (window, Object, Function etc...).
It's means that now "window" object is typed (Window interface), Typescript will not allow you to write this code:

    window.App = {};
    
    error TS2094: The property 'App' does not exist on value of type 'Window'.

The error is quite clear. You can't add dynamically a new property to a typed object.
However, there is a trick that does the job:

    window['App'] = {};

## Cool but...
I think there are some problems with the use of theses declarations files.
The Javascript world is huge. One day, you will want to use a Javascript library that don't have any declaration files.
If you want to have type checking on this library, you will have no choice that write it yourself for *different versions* of the library.
Indeed, each time you release a new version of your library, you have to update the declaration file.
That can be very tedious.

Regarding to the declaration file "lib.d.ts", this one is provided with the Typescript compiler and has the hard mission to define all ECMAScript APIs.
More exactly, it defines the ECMAScript specifically for several versions of Internet Explorer.
So, Typescript compiler will throw you an error whenever you use a feature that is unavailable in IE but not necessarily in others browsers.
I can understand that it would be very hard (even impossible ?) to maintain a declaration file that define ECMAScript for each browser and each version of them.
That why, I have some doubts about the use of this file.

Another annoying point is that we don't know where to get a declaration file for a specific version of one Javascript library.
One anwser could be to create a kind of package manager devote to declaration files.
It would be very cool to have:

    tdm install underscore //Stand for "Typescript declaration files manager"

## Typescript in the real world.
Despite of theses drawbacks, I think I found a way to make Typescript more confortable to use.
That's what we are talking about from now !

### Environnement developpment.
Having a cosy environnement developpment is very important to be effective.
But it's more a personnal choice that an universal rule.
Nevertheless, I give you below a description of my own environnement:

* I don't use Visual Studio.
* I don't care about autocompleting.
* I just code with a text editor (emacs).
* I set up the [Typescript emacs mode](https://github.com/srenault/typescript-mode) provided by Microsoft.
* I use grunt combined with [grunt-typescript](https://github.com/k-maru/grunt-typescript), [grunt-contrib-watch](https://github.com/gruntjs/grunt-contrib-watch) and others plugins to build my Typescript base code.

#### Grunt project skeletons
My "[typescript-project-skeletons](https://github.com/srenault/typescript-project-skeletons)" repository presents two kinds of Typescript projects configuration.
The [first one](https://github.com/srenault/typescript-project-skeletons/tree/master/no-server) is a simple static project (HTML, Typescript and CSS).
The [second one](https://github.com/srenault/typescript-project-skeletons/tree/master/with-play) offers one valid way to intergrate Typescript code into a play framework project.  
I suggest you take a moment to understand how theses projects are organized and build.

*Note: I aware of some [playframework sbt plugins](https://github.com/mumoshu/play2-typescript) that compile Typescript, but I prefer use some front technology like "grunt" to manage my "front" base code.*

### Clever type checking.
My idea is to make a compromise and use type checking only on the code I write.
I don't want to be bothered by any outdated declaration files.
Here how I proceed to get whant I want:

* Declare external Javascript libraries as "any" type.
  That's done like this:

        declare var _: any;
        _.anything; //No compile error.

* Don't use any declaration files for external Javascript libraries.

* Don't use the default provided "lib.d.ts" declaration file.
  The [grunt-typescript](https://github.com/k-maru/grunt-typescript) plugin offers an option named "nolib".
  When set to "true", the declaration file "lib.d.ts" is not used during the Typescript compilation.

* Like external Javascript libraries, declare "window" object and its properties as "any" type.
  This can be done easily thanks to this [tool](). This one will generate for you the corresponding declaration file.

### Functionnal programming.
Typescript can give more than just typing to Javascript.
My idea is to provide to Typescript developper a *core set of functionnal libraries*.
I already [started](https://github.com/srenault/typescript-fp) to create a functionnal library that offer immutable structure like "List" and "Option".
It's just an inception so, feel free to help me on this task.

## The End.
I see Typescript as a real alternative to create front-end application.
The fact that it is a superset of Javascript is its biggest strength.
In my opinion, It has more chance to be adopted by front-end developpers than any others languages that compile to Javascript.
Good luck !
