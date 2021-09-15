# CONCEPT UNDER DEVELOPMENT
## CONCEPT UNDER DEVELOPMENT
### CONCEPT UNDER DEVELOPMENT
#### CONCEPT UNDER DEVELOPMENT
##### CONCEPT UNDER DEVELOPMENT
![selfy](docs/images/selfy.gif)
#### [CHANGELOG](CHANGELOG.md) and BUILD ACTION STATUS
---


## NAME
   easy -  an example github fpm repository

## TOPICS

  * using `easy` to start a new fpm(1) github repository
  * basic fpm usage
  * basic git usage
  * github actions to verify repository changes
  * using __ford(1)__ for documentation
  * Adding plug-ins
     + fpm-man
     + fpm-search
  * registering your project in the fpm(1) registry
  * resources
  * references

## DESCRIPTION

You might be starting off small, but you think your next Fortran project
could really grow into something big.

Well, it can be hard enough just writing your Fortran code.

But you have this nagging feeling you should be using version control,
that you will want collaborators to be able to work with you on the
project, that you should be generating developer documentation as well as
user documention for the code, that you want to easily share your results,
and everyone says you should be building in unit testing of your code
as you develop it. If so, you want the test runs and builds and document
generation to run automatically, of course. 

And then there are all those switches on the compilers, and your code
will likely require a build tool as it grows in complexity, and you
would like to easily incorporate existing code into yours so you are
not spending precious time reinventing the wheel.

It can be hard to get started on the right foot.

You've maybe heard of github and gitlab and git(1) and hg(1), rumblings
about Jenkins and github actions and TOML and YAML configuration files and
markdown, LaTex, and HTML. fpm(1) and Cmake and make(1) and doxygen(1)
and ford(1) have probably come up too. And this all has to work with
the NAG and Intel and NVidia and GNU compilers just to start with; and
maybe Cray/HPE and IBM sooner than later, and so on. Sheesh. You just
want to write some Fortran code, right?

So lets try to provide the minimal guide to get started that out of the
box has you in a position to satisfy all those requirements.

## Getting started

We're going to make some selections for you, but for now we'll assume you
are starting out with nothing but git(1) and a Fortran compiler and a CLI
(Command Line Interface, typically a terminal emulator running a shell).

### create a new github repository from this template

You will need a [github.com account](https://github.com). That is pretty easy.
Go to the github site and create an account.

Now go to this site and click on "use this template" and create a new
repository on your github site.

When you pick the name at the prompt it is best to keep it a lowercase
name that can also be a Fortran variable name. That is, use a-z, 0-9,
and underscores (or dashes if you must) and start it with a letter. We
will assume you changed the directory name to "project1", and that your
github repository name is "johndoe".

Except for perhaps selecting a new license file just stick to the
basics at this juncture unless you have done this before.

Now, in your CLI go to the directory where you want to create your
Fortran project and run
```bash
    git clone https://github.com/johndoe/project1.git 
```
Enter the directory and edit the file "fpm.toml" and change the metadata
at the top to reflect your name and project. The critical line to change
is the 'name="easy"' line; where you should change "easy" to your chosen
project name (ie. "project1" in this example).

Even without a compiler or `fpm` available you can now start changing the
sample code, and then push the changes back to the repository where they
will be compiled with several compilers and be tested with the "fpm test"
command. In addition, developer documentation will be generated using
`ford`. All that will happen automatically. Lets assume you change the
file in app/main.f90. Then you enter the following from within your 
project directory:

```bash
   git add app/main.f90
   git commit -m 'test using my repository'
   git push
```
If you go to your repository site you will see the results of the tests
in the CHANGELOG.md file.

## BUILDING with FPM

Well, that shows a lot of things are in place already, but you do not
want to have to push every change to your web site, so you need to
just do that when you are ready with a new version. Back at the CLI
you would just enter
```bash
     fpm test
```
If `gfortran` is not your default compiler you want to set the environment
variable FPM_COMPILER. In bash(1) shells you might enter something like
```bash
export FPM_COMPILER=ifort
```
so you will not to keep adding "--compiler ifort" to all the `fpm` commands
that need to know which compiler to use (run, test, build, install, ...).

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

```

## DEVELOPER DOCUMENTATION

### ford

- [ford(1) output](https://urbanjost.github.io/easy/fpm-ford/index.html).

## SEE ALSO

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

Next: Typical use

CC BY   Karl Broman


git/github guide
Routine use of git and github

The routine use of git involves just a few commands: principally add, commit, and push, but also status and diff.

You can deal with git and github via a GUI, but I prefer the command line, and so that’s all I’ll discuss.
Add and commit

After you’ve made some small modifications to your project and checked that they work, use git add to indicate that they’re ready.

$ git add R/modified.R man/modified.Rd

Then use git commit to add the modifications to the repository.

$ git commit

A text editor (e.g., emacs) will open; add a short message describing the changes.

To abandon your commit, exit the editor without adding text.

Note that git add is used to add completely new files as well as to “add” modifications to files that already exist in the repository.

The commit message should be short (40 or 60 characters) so it’s easy to read in a list. For a more complex commit, write an initial line that is short and gives the overall idea, followed by as many lines as you want giving the details.

People tend to write commit messages in the present rather than past tense (eg, “Fix such and such” rather than “Fixed such and such”).

For a one-line commit message, you can skip the text editor business and just type

$ git commit -m "Fix such and such"

Add everything

If you want to commit all of the modifications you’ve made, without having to explicitly “add” each file, you can skip the separate add and commit commands and just type

$ git commit -a

I try to avoid this, as it can lead to mistakes (committing more modifications than intended).
Push to github

To push committed changes to github, type

$ git push

You don’t need to do this every time. Do it after you’ve completed a batch of changes that you’re thoroughly happy with and before you move on to something else.

Once you’ve pushed a commit, it’s hard to take it away. If you’ve not pushed it yet, you can go back and scrap it and not have it be part of your project’s history.

But if you move on to something else without having pushed the changes, they may not get to github for months.
Status

You’ve made some changes to a project, but you’re not sure what. Type

git status

It’ll give you a list of files that have been changed, plus new files that haven’t been formally added.
Diff

Exactly what changes have you made? Type

git diff

Or to see your changes to a particular file, type

git diff R/modified.R

It’ll show you which lines have been added and which have been deleted.
How often to commit?

I prefer to do many small commits, each for a set of related changes:

    Think of something that needs to be fixed, or a feature to add.
    Do the work.
    Test that it is okay.
    Add and commit.

Look at others’ projects on github, to see what they do and what sort of commit messages they write.
What to commit?

Don’t include files that are derived from other files in the repository. (Are you using make or rake? You should be! See my make tutorial.)

For example, for a LaTeX manuscript, I wouldn’t include all the .log, .dvi, .aux, etc., files. And if I have R code to generate a figure, I’ll include the R code but not the figure.

Be careful about committing binary files, or really big files. Git works best with text files (like source code), as you can see just the lines that were changed. A new copy of a file will get added to the repository every time you change it. For small text files, that’s no big deal; for big images, you’ll get a bloated repository.

And once you’ve committed a big file to your repository, it’s there forever, even if you use git rm to remove it later.

For big data files that are changing, you’ll want to track a text-based version (not .xls!), and you may want to make a fully separate git repository for the data.
.gitignore

The various files in your project directory that you’re not tracking in git should be indicated in a .gitignore file.

You don’t have to have a .gitignore file, but if you don’t, those files will show up every time you type git status.

Each subdirectory can have its own .gitignore file, too.

Also, you can have a global such in your home directory; I use ~/.gitignore_global, which contains:

*~
.*~
.DS_Store
.Rhistory
.RData

You have to tell git about the global .gitignore file:

$ git config --global core.excludesfile ~/.gitignore_global

Next: Start a new repository

CC BY   Karl Broman

git/github guide
Start a new git repository

Your first instinct, when you start to do something new, should be git init. You’re starting to write a new paper, you’re writing a bit of code to do a computer simulation, you’re mucking around with some new data … anything: think git init.
A new repo from scratch

Say you’ve just got some data from a collaborator and are about to start exploring it.

    Create a directory to contain the project.
    Go into the new directory.
    Type git init.
    Write some code.
    Type git add to add the files (see the typical use page).
    Type git commit.

The first file to create (and add and commit) is probably a ReadMe file, either as plain text or with Markdown, describing the project.

Markdown allows you to add a bit of text markup, like hyperlinks, bold/italics, or to indicate code with a monospace font. Markdown is easily converted to html for viewing in a web browser, and GitHub will do this for you automatically.
A new repo from an existing project

Say you’ve got an existing project that you want to start tracking with git.

    Go into the directory containing the project.
    Type git init.
    Type git add to add all of the relevant files.
    You’ll probably want to create a .gitignore file right away, to indicate all of the files you don’t want to track. Use git add .gitignore, too.
    Type git commit.

Connect it to github

You’ve now got a local git repository. You can use git locally, like that, if you want. But if you want the thing to have a home on github, do the following.

    Go to github.
    Log in to your account.
    Click the new repository button in the top-right. You’ll have an option there to initialize the repository with a README file, but I don’t.
    Click the “Create repository” button.

Now, follow the second set of instructions, “Push an existing repository…”

$ git remote add origin git@github.com:username/new_repo
$ git push -u origin master

Actually, the first line of the instructions will say

$ git remote add origin https://github.com/username/new_repo

But I use git@github.com:username/new_repo rather than https://github.com/username/new_repo, as the former is for use with ssh (if you set up ssh as I mentioned in “Your first time”, then you won’t have to type your password every time you push things to github). If you use the latter construction, you’ll have to type your github password every time you push to github.

Next: Contribute to someone’s repository

CC BY   Karl Broman

REFERENCES

Fortran references
   compiler manuals
   Fortran wiki, discourse, and newsgroup
git
   markdown
   github pages and using HTML and HTML in markdown
fpm
ford
github actions
   Automating your call to fpm test
   Automating your page deployment  https://github.com/JamesIves/github-pages-deploy-action
