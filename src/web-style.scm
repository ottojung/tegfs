;;;; Copyright (C) 2022, 2023  Otto Jung
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

button, input, label, textarea {
	font-family: sans-serif;
	font-size: 130%;
}

textarea {
	padding: 0.5rem;
}

.centering-container {
	display: flex;
	align-items: center;
	justify-content: center;
	height: calc(100% - 4.2rem);
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
	padding-left: 1.25rem;
	padding-right: 0.31rem;
}

.split-container.with-separator .split-left {
	margin-right: 1.25rem;
	padding-left: 0.31rem;
}

.bordered {
	border: 0.125rem solid #f1f1f1;
}

.smooth-edged {
	border-radius: 0.625rem;
}

.with-shadow {
	box-shadow: 0 0.28rem 0.57rem 0 rgba(0, 0, 0, 0.2), 0 0.42rem 1.42rem 0 rgba(0, 0, 0, 0.19);
}

.tiled {
	padding-top: 0.5625rem;
	padding-bottom: 0.8125rem;
	padding-left: 1rem;
	padding-right: 1rem;
}

.tiled.dark {
	background: linear-gradient(#24313c, #435259);;
	box-shadow: 0 0.28rem 0.57rem 0 rgba(0, 0, 0, 0.2), 0 0.42rem 1.42rem 0 rgba(0, 0, 0, 0.19);
}

.tiled.dark, .tiled.dark input, .tiled.dark button {
	color: white;
}

.tiled.dark input[type=text], .tiled.dark input[type=password], .tiled.dark button {
	background-color: rgba(0, 0, 0, 0.1);
}

.tiled.dark button {
	background-color: #343bcb;
	box-shadow: 0 0.28rem 0.57rem 0 rgba(0, 0, 0, 0.2), 0 0.42rem 1.42rem 0 rgba(0, 0, 0, 0.19);
}

.tiled.dark button:hover {
	background-color: #890fdf;
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
	margin-left: 0.625rem;
	letter-spacing: 0.16rem;
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
	border-radius: 0.25rem;
	box-sizing: border-box;
	padding: 0.75rem 1.25rem;
}

input[type=text], input[type=password] {
	width: 100%;
	padding: 0.75rem 1.25rem;
	margin: 0.5rem 0;
	display: inline-block;
	border: 1px solid #ccc;
	box-sizing: border-box;
}

button[type=submit] {
	font-size: 140%;
	border: 0;
	border-radius: 0;
	padding: 0.93rem 0;
	text-transform: uppercase;
	letter-spacing: 0.1rem;
	transition: all.5s ease;
}

.imgcontainer {
	text-align: center;
	margin: 1.5rem 0 0.75rem 0;
}

.copytext {
	font-family: monospace;
	font-size: 180%;
}

.capped-width {
	max-width: 80%;
}

#dangerous {
	margin-top: 0.4rem;
	background-color: #c02525;
}

#dangerous:hover {
	background-color: #e52020;
}

/************
 * Tags box *
 ************/

.tagsbox {
	display: flex;
	align-items: center;
	justify-content: center;
	height: 100%;
	width: 100%;
	margin-top: 0.3rem;
}

.tagsbox div {
	margin-left: 1rem;
	margin-right: 1rem;
}

.tagsbox input[type='checkbox'] {
	display: none;
}

.tagsbox input[type='checkbox'] + label:before {
	width: 1rem;
	height: 1rem;
	background-color: white;
}

.tagsbox input[type='checkbox']:checked + label {
	background-color: #8ddf36;
	color: black;
}

.tagsbox label {
	padding: 0.7rem;
	border-radius: 5rem;
	margin: 0.3rem;
	display: inline-block;
	background-color: #343bcb;
	color: white;
}

/***********************
 * Search input styles *
 ***********************/

.search-input {
	display: flex;
	justify-content: center;
}

.search-input input[type=text] {
	border-width: 0.31rem;
	border-radius: 2.19rem;
	border-color: #1ab188;
}

.search-input input[type=image] {
	margin-left: 0.94rem;
	width: 3.75rem;
}

.search-input .tiled {
	width: 50rem;
}

/*****************
 * Header styles *
 *****************/

header {
	box-shadow: 0 0.28rem 0.57rem 0 rgba(0, 0, 0, 0.2), 0 0.42rem 1.42rem 0 rgba(0, 0, 0, 0.19);
	border-color: black;
	background-color: #24313c;
	text-align: center;
	margin: 0 auto;
	height: 4.2rem;

	border-bottom-left-radius: 9999rem;
	border-bottom-right-radius: 9999rem;
	width: 77rem;
}

header nav ul {
	list-style: none;
	margin: 0;
	padding: 0;
	display: flex;
	justify-content: center;
}

header nav ul a {
	padding-left: 4rem;
	padding-right: 4rem;
	display: flex;
	align-items: center;
	text-align: center;
	text-decoration: none;
}

header nav ul a li {
	display: block;
	font-size: 140%;
	font-family: sans-serif;
	color: white;
	text-decoration: none;
}

.highlighted:hover {
	background-color: rgb(0, 0, 0, 0.1);
}

header nav ul label {
	color: #8ddf36;
	font-size: 70%;
	margin-left: -6rem;
	margin-top: 2.6rem;
	display: block;
}

header nav ul img {
	padding-top: 0.32rem;
	height: 3.5rem;
	transition: transform .1s;
}

header nav ul img:hover {
	transform: scale(1.05);
}

/***************
 * Grid styles *
 ***************/

.card {
	padding: 1rem;
	min-height: 4rem;
	text-align: center;
	max-width: 40vw;
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
	grid-template-columns: repeat(auto-fit, minmax(18.75rem, 1fr));
}

.card img {
	width: 100%;
	margin-bottom: 0.7rem;
}

/*****************
 * Details table *
 *****************/

.details-table {
	width: 40rem;
}

.styled-table {
	border-collapse: collapse;
	margin: 1.56rem 0;
	min-width: 25rem;
	box-shadow: 0 0 1.25rem rgba(0, 0, 0, 0.15);
}

.styled-table tbody {
	background-color: #009879;
	color: #ffffff;
	text-align: left;
}

.styled-table th,
.styled-table td {
	padding: 0.75rem 0.94rem;
}

.styled-table tbody tr {
	border-bottom: 1px solid #dddddd;
}

.styled-table tbody tr:nth-of-type(even) {
	background-color: #f3f3f3;
}

.styled-table tbody tr:last-of-type {
	border-bottom: 0.125rem solid #009879;
}

.styled-table tbody tr.active-row {
	font-weight: bold;
	color: #009879;
}

")
