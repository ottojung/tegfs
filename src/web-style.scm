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
	padding-top: 10px;
	padding-bottom: 10px;
}

.split-left {
	width: 50%;
	padding: 20px;
}

.split-right {
	width: 50%;
}

.split-container.with-separator .split-right {
	border-left: 1px solid #aaa;
	padding-left: 20px;
}

.bordered {
	border: 2px solid #f1f1f1;
	padding: 10px;
}

.smooth-edged {
	border-radius: 10px;
}

.with-shadow {
	box-shadow: 0 4px 8px 0 rgba(0, 0, 0, 0.2), 0 6px 20px 0 rgba(0, 0, 0, 0.19);
}

.form-group label {
	display: block;
	margin-left: 10px;
	margin-top: 10px;
	color: rgba(0, 0, 0, 0.6);
	font-size: 12px;
	font-weight: 500;
	letter-spacing: 0.2em;
	text-transform: uppercase;
}

.form-group input {
	/* outline: none; */
	background: rgba(0, 0, 0, 0.1);
	width: 100%;
	border: 0;
	border-radius: 4px;
	box-sizing: border-box;
	padding: 12px 20px;
	color: rgba(0, 0, 0, 0.6);
	font-family: inherit;
	font-size: inherit;
	line-height: inherit;
}

.centering-container input[type=text], .centering-container input[type=password] {
	width: 100%;
	padding: 12px 20px;
	margin: 8px 0;
	display: inline-block;
	border: 1px solid #ccc;
	box-sizing: border-box;
}

.centering-container button {
	background-color: #04AA6D;
	color: white;
	padding: 14px 20px;
	margin: 8px 0;
	border: none;
	cursor: pointer;
	width: 100%;
}

.centering-container hr {
	border-color: #f1f1f1;
	background-color: #f1f1f1;
	margin-top: 14px;
	margin-bottom: 19px;
	height: 0px;
}

.centering-container button:hover {
	opacity: 0.8;
}

.imgcontainer {
	text-align: center;
	margin: 24px 0 12px 0;
}

.container {
	padding: 16px;
}

span.psw {
	float: right;
	padding-top: 16px;
}

/***************
 * Grid styles *
 ***************/

.card {
	background-color: dodgerblue;
	color: white;
	padding: 1rem;
	min-height: 4rem;
	text-align: center;
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
