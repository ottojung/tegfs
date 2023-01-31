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

(cond-expand
 (guile
  (define-module (tegfs web-fileaudio-svg)
    :export (web::fileaudio.svg)
    :use-module ((tegfs web-define-static-file) :select (web::define-static-file))
    )))

(define web::fileaudio.svg::string
  "<?xml version='1.0' encoding='UTF-8' standalone='no'?>
<!--part of the rodentia icon theme by sixsixfive released under CC0 (https://creativecommons.org/publicdomain/zero/1.0/) on openclipart-->
<svg xmlns='http://www.w3.org/2000/svg' xmlns:xlink='http://www.w3.org/1999/xlink' viewBox='0 0 48 48'>
 <defs id='0'>
  <linearGradient id='A'>
   <stop id='L' stop-color='#fff' stop-opacity='0.8'/>
   <stop id='M' offset='1' stop-color='#fff' stop-opacity='0'/>
  </linearGradient>
  <linearGradient id='B'>
   <stop id='N' stop-color='#bed855'/>
   <stop id='O' offset='1' stop-color='#8bb300'/>
  </linearGradient>
  <linearGradient id='C'>
   <stop id='P' stop-color='#2e3436'/>
   <stop id='Q' offset='1' stop-color='#2e3436' stop-opacity='0'/>
  </linearGradient>
  <radialGradient cx='25.712' cy='48.735' r='21.856' id='D' xlink:href='#C' gradientUnits='userSpaceOnUse' gradientTransform='matrix(1,0,0,0.09243698,0,44.229759)'/>
  <radialGradient cx='13.559' cy='12.06' r='16.219' id='E' xlink:href='#B' gradientUnits='userSpaceOnUse' gradientTransform='matrix(0.6174379,2.5604989,-3.7886681,0.9135993,49.367351,-37.86865)'/>
  <linearGradient y1='2.438' x2='0' y2='43.34' id='F' xlink:href='#A' gradientUnits='userSpaceOnUse'/>
  <radialGradient cx='34.13' cy='8.609' r='3.03' id='G' xlink:href='#B' gradientUnits='userSpaceOnUse' gradientTransform='matrix(1.9999999,2.0717464e-7,-2.1678585e-7,2.0927836,-34.124995,-9.2675658)'/>
  <linearGradient x1='34.13' y1='8.75' x2='36.533' y2='6.363' id='H' xlink:href='#A' gradientUnits='userSpaceOnUse'/>
  <filter x='-0.16' y='-0.151' width='1.321' height='1.302' color-interpolation-filters='sRGB' id='I'>
   <feGaussianBlur stdDeviation='0.5327' id='R'/>
  </filter>
 </defs>
 <path d='m 47.568058,48.734642 a 21.855595,2.0202651 0 1 1 -43.7111893,0 21.855595,2.0202651 0 1 1 43.7111893,0 z' transform='matrix(1.0944165,0,0,0.9661277,-4.1401442,-2.4030967)' id='1' opacity='0.54' fill='url(#D)'/>
 <path d='M 9.84375,1.367375 C 8.1263501,1.367375 6.75,2.7437253 6.75,4.461125 l 0,37.15625 c 0,1.7174 1.3763496,3.093751 3.09375,3.09375 l 28.3125,0 c 1.7174,0 3.09375,-1.376351 3.09375,-3.09375 l 0,-33.21875 -6.75,-7.03125 -24.65625,0 z' id='2' fill='#445800'/>
 <path d='m 9.84375,2.25 c -1.1586131,0 -2.0625,0.903887 -2.0625,2.0625 l 0,37.15625 c 0,1.158614 0.9038868,2.062501 2.0625,2.0625 l 28.3125,0 c 1.158613,0 2.0625,-0.903888 2.0625,-2.0625 l 0,-32.78125 L 34.0625,2.25 9.84375,2.25 z' transform='translate(4.4958504e-8,0.148625)' id='3' fill='url(#E)'/>
 <g transform='translate(2.1311121,-1.402)' id='4'>
  <path d='m 32.059646,13.974322 -13.72025,2.654141 c -0.23057,0.04686 -0.38832,0.248709 -0.38888,0.475487 l -0.01237,14.869132 c -0.72937,-0.503731 -1.64162,-0.804262 -2.63197,-0.804262 -2.387549,0 -4.325339,1.741638 -4.325339,3.886974 0,2.145334 1.93779,3.885206 4.325339,3.885206 2.38755,0 4.32534,-1.739872 4.32534,-3.885206 l 0,-14.09492 11.40816,-2.693837 0,11.116497 c -0.73938,-0.530164 -1.67684,-0.848453 -2.69561,-0.848453 -2.38755,0 -4.32534,1.74164 -4.32534,3.886974 0,2.145335 1.93779,3.886974 4.32534,3.886974 2.38755,0 4.32534,-1.741639 4.32534,-3.886974 l 0.08293,-17.730083 c 0.04565,-0.727609 -0.25282,-0.816656 -0.69269,-0.71765 z' id='J' opacity='0.5' fill='#fff'/>
  <path d='m 32.059646,12.974055 -13.72025,2.654141 c -0.23057,0.04686 -0.38832,0.248709 -0.38888,0.475487 l -0.01237,14.869132 c -0.72937,-0.503731 -1.64162,-0.804262 -2.63197,-0.804262 -2.387549,0 -4.325339,1.741638 -4.325339,3.886974 0,2.145334 1.93779,3.885206 4.325339,3.885206 2.38755,0 4.32534,-1.739872 4.32534,-3.885206 l 0,-14.09492 11.40816,-2.693837 0,11.116497 c -0.73938,-0.530164 -1.67684,-0.848453 -2.69561,-0.848453 -2.38755,0 -4.32534,1.74164 -4.32534,3.886974 0,2.145335 1.93779,3.886974 4.32534,3.886974 2.38755,0 4.32534,-1.741639 4.32534,-3.886974 l 0.08293,-17.730083 c 0.04565,-0.727609 -0.25282,-0.816656 -0.69269,-0.71765 z' id='K' fill='#445800'/>
 </g>
 <path d='m 9.84375,2.75 c -0.8977312,0 -1.5625,0.6647689 -1.5625,1.5625 l 0,37.15625 c 0,0.897732 0.6647689,1.562501 1.5625,1.5625 l 28.3125,0 c 0.897731,0 1.5625,-0.66477 1.5625,-1.5625 l 0,-32.625 -5.875,-6.09375 -24,0 z' transform='translate(4.4958504e-8,0.148625)' id='5' opacity='0.8' fill='none' stroke='url(#F)' stroke-linejoin='round' stroke-linecap='square'/>
 <path d='m 32.25,2.40625 0,6.875 c 0,0.956611 0.676661,1.593751 1.71875,1.59375 l 6.25,0 0,-1.8125 -6.9375,-6.65625 -1.03125,0 z' id='6' opacity='0.16' fill='#2e3436' filter='url(#I)'/>
 <path d='m 33.125,1.375 0,6.3125 c -2e-6,1.158614 0.903887,2.062501 2.0625,2.0625 l 6.0625,0 0,-1.34375 -6.75,-7.03125 -1.375,0 z' id='7' fill='#445800'/>
 <path d='m 34.125,2.40625 0,5.28125 c -10e-7,0.6407004 0.421799,1.0625006 1.0625,1.0625 l 5,0 -6.0625,-6.34375 z' id='8' fill='url(#G)'/>
 <path d='m 34.625,3.6875 0,4 c -10e-7,0.3773049 0.185194,0.5625003 0.5625,0.5625 l 3.84375,0 L 34.625,3.6875 z' id='9' opacity='0.8' fill='none' stroke='url(#H)' stroke-linecap='square'/>
</svg>")

(web::define-static-file
 web::fileaudio.svg
 `(image/svg+xml)
 web::fileaudio.svg::string)
