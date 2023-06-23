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
