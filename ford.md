---
src_dir: src
         app
         example
         test
output_dir: docs/fpm-ford
project: $PUT_YOUR_PROJECT_NAME_HERE
summary: $PUT_YOUR_PROJECT_DESCRIPTION_HERE
project_github: https://github.com/$PUT_YOUR_REPOSITORY_NAME_HERE/$PUT__YOUR_PROJECT_NAME_HERE
project_download:
author: $PUT_YOUR_FULL_NAME_HERE
author_email: $PUT_YOUR_EMAIL_HERE
github: https://github.com/$PUT_YOUR_REPOSITORY_NAME_HERE/$PUT__YOUR_PROJECT_NAME_HERE
media_dir: docs/images
exclude_dir: archive
             FODDER
display: public
         protected
source: true
proc_internals: true
sort: permission-alpha
favicon: docs/images/favicon.ico
print_creation_date: true
extra_mods: iso_fortran_env:https://gcc.gnu.org/onlinedocs/gfortran/ISO_005fFORTRAN_005fENV.html
            tomlf:https://toml-f.github.io/toml-f
creation_date: %Y-%m-%d %H:%M %z
md_extensions: markdown.extensions.toc
               markdown.extensions.smarty
---
