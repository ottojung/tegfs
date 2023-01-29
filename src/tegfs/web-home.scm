;;;; Copyright (C) 2023  Otto Jung
;;;;
;;;; This program is free software: you can redistribute it and/or modify
;;;; it under the terms of the GNU Affero General Public License as published
;;;; by the Free Software Foundation, either version 3 of the License, or
;;;; (at your option) any later version.
;;;;
;;;; This program is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;; GNU Affero General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU Affero General Public License
;;;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

%run guile

%var web::home

%use (web::make-html-response) "./web-make-html-response.scm"

(define (web::home::page)
  (display "
<div class='homepage'>
  <img src='static/logo-white.jpeg'>
</div>
<div class='homepage'>
  <p>
    Welcome to TegFS, the ultimate file tagging system! With TegFS, organizing your files has never been easier.
    Its user-friendly CLI and web UI make tagging and searching for your files a breeze.
    And the best part? It's completely free and libre software, made by community for the community.

    <br/>
    <br/>

    If you are a registered user, then you can access the full functionality of TegFS <a href='auth?yes=query'>here</a>.
    After the login, you can search for files using tags, upload new files, and share them with others.

    <br/>
    <br/>

    If you are not registered and you got the link, then note that this is a private page,
    and the link you got is owned by the registered person that shared it with you.

    <br/>
    <br/>

    But that's not all, TegFS is a community-driven project, which means that everyone can <a href='https://codeberg.org/otto/tegfs/pulls'>contribute</a>,
    <a href='https://codeberg.org/otto/tegfs/issues'>report bugs</a>, and <a href='https://codeberg.org/otto/tegfs/issues'>request new features</a>.
    With your help, we can make TegFS an even more powerful tool for everyone.
    Happy tagging!

  </p>
</div>"))

(define (web::home)
  (web::make-html-response
   web::home::page
   #:display-header? #f))
