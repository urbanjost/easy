# CONCEPT UNDER DEVELOPMENT
## CONCEPT UNDER DEVELOPMENT
### CONCEPT UNDER DEVELOPMENT
![selfy](docs/images/selfy.gif)
#### [CHANGELOG](docs/CHANGELOG.md) and BUILD ACTION [STATUS](docs/STATUS.md)
---

## Name
   easy -  an example github fpm repository

## Topics

  * using `easy` to start a new fpm(1) github repository
  * basic fpm usage
  * basic git usage
  * github actions to verify repository changes
  * using __ford(1)__ for documentation
  * Adding plug-ins
     + fpm-man
     + fpm-search
  * github topics
  * registering your project in the fpm(1) registry
  * resources
  * references

## Description

You might be starting off small, but you think your next Fortran project
could really grow into something big.

Well, it can be hard enough just writing your Fortran code.

But you have this nagging feeling you 
+ should be using version control
+ will want collaborators to be able to work with you on the project
+ should be generating developer documentation as well as user documention
  for the code
+ want to easily share your results
+ and everyone says you should be building in unit testing of your code
  as you develop it

You want the test runs and builds and document generation to run
automatically, of course.

And then there are all those **compilers switches**, and your code will
likely require a **build tool** as it grows in complexity, and you would
like to easily **incorporate existing code** into yours so you are not
spending precious time reinventing the wheel.

It can be hard to get started on the right foot.

You've maybe heard of github and gitlab and git(1), svn(1) and hg(1), rumblings
about Jenkins and github actions and TOML and YAML configuration files and
markdown, LaTex, and HTML. fpm(1) and Cmake and make(1) and doxygen(1)
and ford(1) have probably come up too. And this all has to work with
the NAG and Intel and NVidia and GNU compilers just to start with; and
maybe Cray/HPE and IBM sooner than later, and so on. 

Sheesh. You just
want to write some Fortran code, right?

So lets try to provide the minimal guide to get started that out of the
box has you in a position to satisfy all those requirements.

## Getting started

We're going to make some selections for you, but for now we'll assume you
are starting out with nothing but git(1) and a Fortran compiler and a CLI
(Command Line Interface, typically a terminal emulator running a shell).

We are also assuming your project is standard Fortran and does not
depend upon external libraries yet, although how to add external system
libraries and other fpm packages via your fpm.toml file is described in
the manifest file guide on the fpm(1) site (one of those decisions we are
making for you is going to be to use **git** and **github** and **fpm**,
the Fortran Package Manager).

### create a new github repository from this template

**You will need a [github.com account](https://github.com)**. That is pretty easy.
Go to the github site and create an account.

Now go to [this site](https://github.com/urbanjost/easy)(https://github.com/urbanjost/easy)
and click on **"use this template"** and create a new repository on your
github site.

When you pick the new name for your repository at the prompt it is best to
keep it a lowercase name that can also be a Fortran variable name. That
is, use a-z, 0-9, and underscores (or dashes if you must) and start
it with a letter. **We will assume you changed the directory name to
"project1", and that your github repository name is "johndoe"**. **Use
the actual names you selected in the following examples**.

Except for perhaps selecting a new license file just stick to the
basics at this juncture unless you have done this before.

But on your new site, go to **"settings"** and under **"Github Pages"** select
the **"main"** branch, and the **"doc"** directory and click on **"save"**.

You are done with the initial setup. You have already completed more of the
stated goals than you imagine. Lets move off the web site and back to your
platform.

Now, in your CLI go to the directory where you want to create your
Fortran projects and make a directory called **"github"** and enter it to run
```bash
    git clone https://github.com/johndoe/project1.git 
```
Enter the **"project1"** directory and edit the file **"fpm.toml"** and change
the metadata at the top to reflect your name and project. The critical
line to change is the **'name="easy"'** line; where you should change "easy"
to your chosen project name (ie. "project1" in this example).

Now edit the **CHANGELOG.md** file and replace **"YOUR_*"** names
with your github repository name and your project name and so on. The
strings to change are YOUR_PROJECT_NAME, YOUR_FIRSTNAME, YOUR_LASTNAME,
YOUR_SITE_NAME, YOUR_REPOSITORY_NAME.

Do the same with the **STATUS.md** file as well.

Even without a compiler or `fpm` available you can now start changing the
sample code, and then optionally push the changes back to the repository where they
will be compiled with several compilers and be tested with the "fpm test"
command automatically. In addition, developer documentation will be generated using
`ford`. 

Lets give that a go before we move on in the CLI.  Lets assume you change
the file in app/main.f90. Adding a **"print *, 'my first change'**
is sufficient.  Then you enter the following from within your project
directory:

```bash
   # show the files you have changed
   git status
   # show the changes in detail
   git diff

   # add the changed files to the list of files to commit to your new version
   git commit --all      # adds all the files you changed

   # ALTERNATIVELY, individually add the file(s) you changed
   # optionally you can list the files by name with add command(s)
   git add fpm.toml app/main.f90 CHANGELOG.md
   # create your new version ( just "git commit" will take you into an editor
   # where you can create a more complete description of your changes).
   git commit -m 'test using my repository'

   # push the changes back to your github repository
   git push
```
If you go to your github repository site you will see the results of
the tests in the STATUS.md file. You can also use the **"Actions"**
button at the top. Hopefully, everything was compiled on several platforms
and the "fpm test" command was run on each platform.

## Building with fpm(1)

Well, that shows a lot of things are in place already, but you do not
want to have to push every change to your web site, so you need to
just do that when you are ready with a new version. Back at the CLI
you would just enter
```bash
     fpm test
```
But __first__, if `gfortran` is not your default compiler you want to
set the environment variable FPM_COMPILER. In bash(1) shells you might
enter something like
```bash
export FPM_COMPILER=ifort
```
Now you will not have to keep adding "--compiler ifort" to all the `fpm`
commands that need to know which compiler to use (run, test, build,
install, ...). The value you have to set is probably just your compiler
name.

So assuming your github repository is public others can now use your
code as an `fpm` dependency by using it in their fpm.toml file using
something like
```toml
     [dependencies]
     M_time        = { git = "https://github.com/$YOUR_REPRO/project1.git" }
```
and at your discretion others can now collaborate with you on its
development via the WWW (or an internal github server in a very
similiar manner). Developer documentation if being built via `ford`
in the repository directory docs/fpm-ford, and your tests are being
run on Ubuntu, MSWindows, and MacOS systems with gfortran, and ifort on
Ubuntu automatically.

Hopefully, this is starting to feel like progress.

## Developer Documentation

### ford

- [ford(1) output](https://urbanjost.github.io/easy/fpm-ford/index.html).

## See Also

 * [Fortran Package Manager](https://github.com/fortran-lang/fpm) )


Discussion is welcome here as well as at
 - [Fortran Discourse](https://fortran-lang.discourse.group) is a resource for discussion
   Fortran 
 - [Google Fortran newsgroup](https://groups.google.com/forum/#!forum/comp.lang.fortran) is
   another popular Fortran forum
 - The [Fortran Wiki](http://fortranwiki.org) contains information on many Fortran resources
 - Fortran compiler sites have Fortran reference manuals available
 - [Fortran Package Manager](https://github.com/fortran-lang/fpm) has a growing list of
   resources including how to setup Fortran compilers, fpm(1)


1. Fortran compilers
2. Fortran references
3. git
4. fpm

    Your first time: get github account; install git, set up ssh.
    Typical use: add, commit, push, status, and diff.
    Start a new repository: from scratch, or with an existing project.

Version control is the only reasonable way to keep track of changes in code, manuscripts, presentations, and data analysis projects.

Your first time with git and github

If you’ve never used git or github before, there are a bunch of things
that you need to do. It’s very well explained on github, but repeated
here for completeness.

    Get a github account.
    Download and install git.

    Set up git with your user name and email.

        Open a terminal/shell and type:

        $ git config --global user.name "Your name here"
        $ git config --global user.email "your_email@example.com"

        (Don’t type the $; that just indicates that you’re doing this at the command line.)

        I also do:

        $ git config --global color.ui true
        $ git config --global core.editor emacs

        The first of these will enable colored output in the terminal; the second tells git that you want to use emacs.

    Set up ssh on your computer. I like Roger Peng’s guide to setting up password-less logins. Also see github’s guide to generating SSH keys.
        Look to see if you have files ~/.ssh/id_rsa and ~/.ssh/id_rsa.pub.

        If not, create such public/private keys: Open a terminal/shell and type:

        $ ssh-keygen -t rsa -C "your_email@example.com"

        Copy your public key (the contents of the newly-created id_rsa.pub file) into your clipboard. On a Mac, in the terminal/shell, type:

        $ pbcopy < ~/.ssh/id_rsa.pub

    Paste your ssh public key into your github account settings.
        Go to your github Account Settings
        Click “SSH Keys” on the left.
        Click “Add SSH Key” on the right.
        Add a label (like “My laptop”) and paste the public key into the big text box.

        In a terminal/shell, type the following to test it:

        $ ssh -T git@github.com

        If it says something like the following, it worked:

        Hi username! You've successfully authenticated, but Github does
        not provide shell access.

## git/github

As you collaborate with others and start releasing official versions you will use more and more
git commands, but initially you can with by with adding, committing, and pushing your changes.
Let's make you master a few concepts as covered in this [git/github tutorial](tutorials/git_github.md).

## markdown
## github pages and using HTML and HTML in markdown
## Automating your call to fpm test
## Automating your page deployment  https://github.com/JamesIves/github-pages-deploy-action

## github topics

Any project complete enough should generate a github **Release** and add the topic "fortran-package-manager"
to their github repository. 

Putting out a release lets users know you have reached a milestone where something is fully useable.
It is very easy to overlook making a release, but a lot of consumers of github packages consider it a red flag
if no release is present.

Topic names that are well chosen make it much easier to find your project. In particular, fpm(1) projects want
to include the topic [fortran-package-manager](https://github.com/topics/fortran-package-manager). Then, if
you click on the topic you will see a burgeoning list of other fpm(1)-compatible packages.

## Fortran References 
- **fpm resources**
   + [fpm(Fortran Package Manager)](https://github.com/fortran-lang/fpm)
   + [fpm manifest file](https://github.com/fortran-lang/fpm/blob/master/manifest-reference.md) or your fpm.toml file
- **git resources**
- **github resources**
   + [github.com](https://github.com)
   + [github actions](https://docs.github.com/en/actions)
   + [github markdown]
- **Fortran reference materials**
   + [Fortran 2018 Standard](https://j3-fortran.org/doc/year/18/18-007r1.pdf)
   + [GNU gfortran intrinsic descriptions](https://gcc.gnu.org/onlinedocs/gfortran/Intrinsic-Procedures.html)
   + [J3](https://j3-fortran.org)
- **compiler information**
   + [GNU gcc](http://gcc.gnu.org)
- **automatic documentation tools
   + [ford](https://politicalphysicist.github.io/ford-fortran-documentation.html)
   + [doxygen](https://www.doxygen.nl/index.html)
- **Fortran resources**
   + [Fortran Wiki](http://fortranwiki.org) contains information on many Fortran resources
   + [Fortran Discourse](https://fortran-lang.discourse.group)
   + [Google Fortran newsgroup](https://groups.google.com/forum/#!forum/comp.lang.fortran)
   + [Fortran-lang](https://fortran-lang.org)
   + [Fortran Standard Library project](https://github.com/fortran-lang/stdlib)
