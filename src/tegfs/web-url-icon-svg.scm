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

(cond-expand
 (guile
  (define-module (tegfs web-url-icon-svg)
    :export (web::url-icon/svg)
    :use-module ((euphrates stringf) :select (stringf)))))



(define web::url-icon/svg/template
  "<?xml version='1.0' encoding='UTF-8' standalone='no'?>
   <!--part of the matt icon theme by sixsixfive released under CC0 (https://creativecommons.org/publicdomain/zero/1.0/) on openclipart-->
   <svg width='~apx' height='~apx' xmlns='http://www.w3.org/2000/svg' xmlns:xlink='http://www.w3.org/1999/xlink' viewBox='0 0 128 128'>
    <defs id='0'>
     <linearGradient id='2'>
      <stop id='H' stop-color='#efefef'/>
      <stop id='I' offset='1' stop-color='#ddd'/>
     </linearGradient>
     <linearGradient id='3'>
      <stop id='J' stop-color='#6193cf'/>
      <stop id='K' offset='1' stop-color='#00438a'/>
     </linearGradient>
     <linearGradient id='4'>
      <stop id='L'/>
      <stop id='M' offset='1' stop-opacity='0'/>
     </linearGradient>
     <linearGradient id='5'>
      <stop id='N' stop-color='#eee'/>
      <stop id='O' offset='1' stop-color='#dcdcdc'/>
     </linearGradient>
     <radialGradient cx='63.894' cy='117.35' r='53.91' id='6' xlink:href='#4' gradientUnits='userSpaceOnUse' gradientTransform='matrix(1,0,0,0.08641975,0,107.20963)'/>
     <radialGradient cx='60.05' cy='51.39' r='15.03' id='7' xlink:href='#5' gradientUnits='userSpaceOnUse' gradientTransform='matrix(-1.404511,1.1888571,-1,-1.1813958,199.73811,11.750541)'/>
     <radialGradient cx='53.878' cy='49.37' r='33.11' id='8' xlink:href='#2' gradientUnits='userSpaceOnUse' gradientTransform='matrix(1,0,0,1.135114,0,-10.484729)'/>
     <radialGradient cx='64.7' cy='-15.174' r='55.27' id='9' xlink:href='#A' gradientUnits='userSpaceOnUse' gradientTransform='matrix(1.5330966,-3.1358239e-8,2.404893e-8,1.1240308,-33.828167,2.9909361)'/>
     <linearGradient id='A'>
      <stop id='P' stop-color='#eee'/>
      <stop id='Q' offset='1' stop-color='#eee' stop-opacity='0'/>
     </linearGradient>
     <linearGradient y1='8.707' x2='0' y2='120.54' id='B' xlink:href='#3' gradientUnits='userSpaceOnUse'/>
    </defs>
    <g id='1'>
     <path d='m 117.80366,117.35108 a 53.910149,4.6589017 0 1 1 -107.8202997,0 53.910149,4.6589017 0 1 1 107.8202997,0 z' transform='translate(1.2996963,3.4536055)' id='C' opacity='0.443' fill='url(#6)'/>
     <path d='m 122.08898,64.625572 a 56.734097,55.918617 0 1 1 -113.4681952,0 56.734097,55.918617 0 1 1 113.4681952,0 z' transform='matrix(1.0119373,0,0,1.0266947,-2.1350421,-4.4029736)' id='D' fill='url(#B)'/>
     <path d='m 45.75,40.03125 c -0.894755,0.165881 -1.500113,1.001101 -1.40625,1.90625 L 50,99.1875 c 0.07192,0.666745 0.524301,1.21307 1.15625,1.4375 0.63195,0.22443 1.336162,0.097 1.8125,-0.375 -2e-6,0 12.855839,-12.858659 15.0625,-15.0625 8.678049,8.703649 14.676957,19.58218 17.4375,30 C 95.580085,111.10204 104.265,104.21927 110.5625,95.5 105.81331,84.490087 98.606019,73.674589 88.9375,64.3125 l 14.625,-14.625 c 0.46693,-0.471421 0.65266,-1.153579 0.4375,-1.78125 -0.21516,-0.627671 -0.77955,-1.070425 -1.4375,-1.15625 L 46.28125,40.03125 c -0.176731,-0.02377 -0.355564,-0.03055 -0.53125,0 z' id='E' fill='url(#8)' display='block' stroke-width='2.093'/>
     <path d='m 122.08898,64.625572 a 56.734097,55.918617 0 1 1 -113.4681952,0 56.734097,55.918617 0 1 1 113.4681952,0 z' transform='matrix(1.0119373,0,0,1.0266947,-2.1350421,-4.4029736)' id='F' fill='none' stroke='#00316e' stroke-width='3.924'/>
     <g transform='translate(-0.00454736,5.8747942e-4)' id='G'>
      <path d='m 65.34375,11.21875 c -29.979853,0 -54.21875,23.915681 -54.21875,53.40625 0,29.490569 24.238897,53.40625 54.21875,53.40625 29.979853,0 54.25,-23.917374 54.25,-53.40625 0,-29.488876 -24.270147,-53.40625 -54.25,-53.40625 z' transform='matrix(1.0119373,0,0,1.0266947,-2.1350421,-4.4029736)' id='R' fill='none' stroke='url(#9)' stroke-width='2.079'/>
     </g>
    </g>
   </svg>")

(define (web::url-icon/svg width height)
  (stringf web::url-icon/svg/template width height))
