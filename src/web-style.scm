;;;; Copyright (C) 2022  Otto Jung
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

%var web::style

(define web::style
  "







html, body {
	height: 100%;
	margin: 0;
	padding: 0;
}

body {
	font-family: sans-serif;
}

button, input, label {
	font-family: sans-serif;
	font-size: 130%;
}

.centering-container {
	display: flex;
	align-items: center;
	justify-content: center;
	height: 100%;
	width: 100%;
}

.split-container {
	display: flex;
	align-items: center;
	justify-content: center;
}

.split-left, .split-right {
	width: 50%;
}

.split-container.with-separator .split-right {
	border-left: 1px solid #aaa;
	padding-left: 20px;
	padding-right: 5px;
}

.split-container.with-separator .split-left {
	margin-right: 20px;
	padding-left: 5px;
}

.bordered {
	border: 2px solid #f1f1f1;
}

.smooth-edged {
	border-radius: 10px;
}

.with-shadow {
	box-shadow: 0 4px 8px 0 rgba(0, 0, 0, 0.2), 0 6px 20px 0 rgba(0, 0, 0, 0.19);
}

.tiled {
	padding-top: 9px;
	padding-bottom: 13px;
	padding-left: 16px;
	padding-right: 16px;
}

.tiled.dark {
	background: linear-gradient(#24313c, #435259);;
	box-shadow: 0 4px 8px 0 rgba(0, 0, 0, 0.2), 0 6px 20px 0 rgba(0, 0, 0, 0.19);
}

.tiled.dark, .tiled.dark input, .tiled.dark button {
	color: white;
}

.tiled.dark input[type=text], .tiled.dark input[type=password], .tiled.dark button {
	background-color: rgba(0, 0, 0, 0.1);
}

.tiled.dark button {
	background-color: #1ab188;
}

.tiled.bright, .tiled.bright input {
	color: rgba(0, 0, 0, 0.6);
}

.tiled-v-element {
	display: flex;
	width: 100%;
	justify-content: center;
}

.form-block label {
	font-size: 80%;
	margin-left: 10px;
	letter-spacing: 0.2em;
	text-transform: uppercase;
}

.form-block input {
	width: 100%;
}

.form-block button {
	width: 100%;
}

.form-block input, .form-block button {
	border: 0;
	border-radius: 4px;
	box-sizing: border-box;
	padding: 12px 20px;
}

input[type=text], input[type=password] {
	width: 100%;
	padding: 12px 20px;
	margin: 8px 0;
	display: inline-block;
	border: 1px solid #ccc;
	box-sizing: border-box;
}

button[type=submit] {
	font-size: 140%;
	border: 0;
	border-radius: 0;
	padding: 15px 0;
	text-transform: uppercase;
	letter-spacing: .1em;
	transition: all.5s ease;
}

hr {
	border-color: #f1f1f1;
	background-color: #f1f1f1;
	margin-top: 14px;
	margin-bottom: 19px;
	height: 0px;
}

.imgcontainer {
	text-align: center;
	margin: 24px 0 12px 0;
}

.copytext {
	font-family: monospace;
	font-size: 180%;
}

/***********************
 * Search input styles *
 ***********************/

.search-input {
	display: flex;
	justify-content: center;
}

.search-input input[type=text] {
	border-width: 5px;
	border-radius: 35px;
	border-color: #1ab188;
}

.search-input input[type=image] {
	margin-left: 15px;
	width: 60px;
}

.search-input .tiled {
	width: 60%;
}

/*****************
 * Header styles *
 *****************/

header {
	box-shadow: 0 4px 8px 0 rgba(0, 0, 0, 0.2), 0 6px 20px 0 rgba(0, 0, 0, 0.19);
	background-color: #24313c;
	color: white;
	padding: 4px;
	text-align: center;
	width: 90%;
	margin: 0 auto;

	border-bottom-left-radius: 15px;
	border-bottom-right-radius: 15px;
}

header nav ul {
	list-style: none;
	margin: 0;
	padding: 0;
	display: flex;
	justify-content: space-around;
}

header nav ul li {
	padding: 0px 50px;
	border-radius: 5px;
	display: flex;
	align-items: center;
	text-align: center;
}

header nav ul li a {
	display: block;
	font-size: 140%;
	font-family: sans-serif;
	color: #fff;
	text-decoration: none;
	border-radius: 4px;
}

header nav ul li label {
	font-size: 80%;
}

header nav ul li:hover {
	background-color: #435259;
}

/***************
 * Grid styles *
 ***************/

.card {
	padding: 1rem;
	min-height: 4rem;
	text-align: center;
}

.card:hover {
	background-color: rgba(1, 0, 0, 0.1);
}

.card a {
	color: black;
	text-decoration: none;
}

.cards {
	max-width: 90%;
	margin: 0 auto;
	display: grid;
	gap: 1rem;
	grid-template-columns: repeat(auto-fit, minmax(300px, 1fr));
}

.card img {
	width: 100%;
	margin-bottom: 0.7em;
}

/*****************
 * Details table *
 *****************/

.styled-table {
	border-collapse: collapse;
	margin: 25px 0;
	min-width: 400px;
	box-shadow: 0 0 20px rgba(0, 0, 0, 0.15);
}

.styled-table tbody {
	background-color: #009879;
	color: #ffffff;
	text-align: left;
}

.styled-table th,
.styled-table td {
	padding: 12px 15px;
}

.styled-table tbody tr {
	border-bottom: 1px solid #dddddd;
}

.styled-table tbody tr:nth-of-type(even) {
	background-color: #f3f3f3;
}

.styled-table tbody tr:last-of-type {
	border-bottom: 2px solid #009879;
}

.styled-table tbody tr.active-row {
	font-weight: bold;
	color: #009879;
}

")
