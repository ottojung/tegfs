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

%use (web::define-static-file) "./web-define-static-file.scm"

(define web::settings-gear-image-string
  "
<?xml version='1.0' encoding='UTF-8' standalone='no'?>
<!--part of the rodentia icon theme by sixsixfive released under CC0 (https://creativecommons.org/publicdomain/zero/1.0/) on openclipart-->
<svg xmlns='http://www.w3.org/2000/svg' xmlns:xlink='http://www.w3.org/1999/xlink' viewBox='0 0 48 48'>
 <defs id='0'>
  <linearGradient id='7'>
   <stop id='J' stop-color='#fff' stop-opacity='0.8'/>
   <stop id='K' offset='1' stop-color='#fff' stop-opacity='0'/>
  </linearGradient>
  <linearGradient id='8'>
   <stop id='L' stop-color='#888a85'/>
   <stop id='M' offset='0.78' stop-color='#555753'/>
   <stop id='N' offset='1' stop-color='#2e3436'/>
  </linearGradient>
  <linearGradient id='9'>
   <stop id='O' stop-color='#2e3436'/>
   <stop id='P' offset='1' stop-color='#2e3436' stop-opacity='0'/>
  </linearGradient>
  <linearGradient id='A'>
   <stop id='Q' stop-color='#fff' stop-opacity='0.45'/>
   <stop id='R' offset='1' stop-color='#fff' stop-opacity='0'/>
  </linearGradient>
  <linearGradient id='B'>
   <stop id='S' stop-color='#fff' stop-opacity='0.679'/>
   <stop id='T' offset='1' stop-opacity='0.333'/>
  </linearGradient>
  <linearGradient y1='3.1' x2='0' y2='122.9' id='C' gradientUnits='userSpaceOnUse'>
   <stop id='U' stop-color='#888a85'/>
   <stop id='V' offset='1' stop-color='#babdb6'/>
  </linearGradient>
  <radialGradient cx='16.791' cy='8.822' r='20.676' id='E' xlink:href='#A' gradientUnits='userSpaceOnUse'/>
  <linearGradient x1='86.13' y1='105.11' x2='84.64' y2='20.895' id='F' xlink:href='#B' gradientUnits='userSpaceOnUse' gradientTransform='matrix(0.2879622,-0.1167595,0.1167595,0.2879622,-1.7854195,12.564044)'/>
  <radialGradient cx='25.25' cy='41.18' r='22' id='G' xlink:href='#C' gradientUnits='userSpaceOnUse' gradientTransform='matrix(0.6210894,-1.8176291,1.6110669,0.5505064,-50.378255,62.138382)'/>
  <linearGradient x1='2.35' y1='1.35' x2='45.65' y2='44.65' id='H' xlink:href='#8' gradientUnits='userSpaceOnUse' gradientTransform='matrix(0.9576328,0,0,0.9576328,1.0168128,1.2074984)'/>
  <linearGradient y1='2.39' x2='0' y2='43.61' id='I' xlink:href='#7' gradientUnits='userSpaceOnUse'/>
 </defs>
 <path d='m 40.251581,46.137615 a 13.817707,1.3817707 0 1 1 -27.635414,0 13.817707,1.3817707 0 1 1 27.635414,0 z' transform='matrix(1.2506696,0,0,2.3511297,-8.9182042,-66.224239)' id='1' opacity='0.628' fill='url(#D)'/>
 <path d='m 13.628904,5.2209981 c -0.750903,0.3044674 -1.106347,1.1739097 -0.796675,1.9376487 l 1.09075,2.6901026 C 12.725361,10.750898 11.661532,11.811438 10.764513,12.970076 L 8.07504,11.832393 C 7.6955317,11.671855 7.3140787,11.66 6.9634447,11.798722 6.6128107,11.937445 6.3080559,12.207425 6.1502171,12.580555 l -2.2259022,5.262012 c -0.3156783,0.74626 0.044611,1.581259 0.8036279,1.902334 l 2.6894731,1.137683 c -0.2067389,1.450635 -0.2269084,2.952658 -0.040237,4.440379 l -2.6901027,1.090752 c -0.7637391,0.309671 -1.1401208,1.170256 -0.8356529,1.92116 l 2.1365221,5.269274 c 0.3044674,0.750902 1.1739098,1.106346 1.9376487,0.796674 l 2.6901029,-1.09075 c 0.902148,1.197618 1.962688,2.261447 3.121326,3.158466 l -1.137683,2.689474 c -0.321074,0.759016 0.0019,1.609144 0.748162,1.924823 l 5.262013,2.225901 c 0.746259,0.315678 1.581257,-0.04461 1.902333,-0.803627 l 1.137684,-2.689474 c 1.450635,0.206739 2.952657,0.226909 4.440379,0.04024 l 1.090751,2.690103 c 0.309672,0.763739 1.170256,1.140121 1.92116,0.835653 l 5.269274,-2.136522 c 0.750903,-0.304467 1.106347,-1.17391 0.796675,-1.937649 l -1.09075,-2.690103 c 1.197618,-0.902148 2.261447,-1.962688 3.158466,-3.121326 l 2.689473,1.137683 c 0.759015,0.321074 1.609145,-0.0019 1.924823,-0.748162 l 2.225902,-5.262012 c 0.315678,-0.74626 -0.04461,-1.58126 -0.803627,-1.902334 l -2.689474,-1.137683 c 0.206739,-1.450636 0.226908,-2.952658 0.04024,-4.44038 l 2.690103,-1.090751 c 0.763739,-0.309672 1.140121,-1.170256 0.835653,-1.92116 l -2.136522,-5.269274 c -0.304468,-0.750903 -1.17391,-1.106346 -1.937649,-0.796675 l -2.690103,1.090751 C 36.482155,11.958413 35.421615,10.894585 34.262977,9.997566 L 35.40066,7.3080928 C 35.721734,6.5490771 35.398759,5.6989474 34.652498,5.3832699 L 29.390485,3.1573677 c -0.74626,-0.3156783 -1.58126,0.044612 -1.902333,0.8036279 L 26.350469,6.6504687 C 24.899833,6.4437298 23.397811,6.4235603 21.910089,6.6102316 L 20.819338,3.920129 C 20.509666,3.1563898 19.649082,2.7800081 18.898178,3.084476 l -5.269274,2.1365221 z m 8.032167,12.2435869 c 3.184193,-1.29109 6.816308,0.245344 8.107397,3.429538 1.291089,3.184194 -0.245345,6.816308 -3.429539,8.107397 -3.184193,1.291089 -6.816308,-0.245344 -8.107397,-3.429539 -1.291089,-3.184193 0.245345,-6.816307 3.429539,-8.107396 z' id='2' fill='url(#G)' display='block' stroke='url(#H)' stroke-linejoin='round' stroke-width='0.958'/>
 <path d='M 19.0625,2.9375 18.90625,3 13.53125,5.15625 C 13.283224,5.2568166 13.180653,5.4873259 13.3125,5.8125 a 0.58589857,0.58589857 0 0 1 0,0.03125 0.58589857,0.58589857 0 0 1 0.03125,0.03125 l 0,0.0625 A 0.58589857,0.58589857 0 0 1 13.375,6 l 1.0625,2.65625 a 0.58589857,0.58589857 0 0 1 0.03125,0 c 0.165011,0.435067 -7.2e-4,0.9422985 -0.375,1.21875 -1.173373,0.883885 -2.179389,1.890577 -3.0625,3.03125 -0.27834,0.398685 -0.802306,0.526888 -1.25,0.34375 L 9.75,13.25 6.96875,12.0625 c -0.1967474,-0.08323 -0.2494233,-0.08093 -0.375,-0.03125 -0.1250534,0.04948 -0.1863495,0.09953 -0.25,0.25 a 0.58589857,0.58589857 0 0 1 -0.0625,0.125 0.58589857,0.58589857 0 0 1 -0.09375,0.125 l -2.21875,5.25 c -0.057691,0.136381 -0.042674,0.222915 0,0.3125 0.042674,0.08958 0.1325647,0.205135 0.3125,0.28125 l 2.8125,1.1875 c 0.4510555,0.18135 0.7042451,0.645356 0.625,1.125 l 0.03125,0 c -0.2007516,1.408623 -0.2105696,2.89216 -0.03125,4.34375 0.00129,0.01044 -0.00131,0.02081 0,0.03125 0.05199,0.472005 -0.243869,0.927532 -0.6875,1.09375 C 7.016858,26.16165 7.014729,26.18272 7,26.1875 l -2.78125,1.09375 c -0.3065179,0.124283 -0.3932285,0.34883 -0.28125,0.625 a 0.58589857,0.58589857 0 0 1 0,0.03125 0.58589857,0.58589857 0 0 1 0.03125,0.03125 l 0,0.03125 A 0.58589857,0.58589857 0 0 1 4,28.0625 l 0,0.03125 2.15625,5.375 c 0.1005671,0.248027 0.3310756,0.350598 0.65625,0.21875 a 0.58589857,0.58589857 0 0 1 0.03125,0 0.58589857,0.58589857 0 0 1 0.03125,-0.03125 l 0.0625,0 A 0.58589857,0.58589857 0 0 1 7,33.625 l 2.65625,-1.0625 a 0.58589857,0.58589857 0 0 1 0,-0.03125 c 0.435067,-0.165011 0.942298,7.2e-4 1.21875,0.375 0.883884,1.173374 1.890577,2.179389 3.03125,3.0625 0.398685,0.27834 0.526888,0.802306 0.34375,1.25 l 0,0.03125 -1.1875,2.78125 c -0.142597,0.337098 -0.07799,0.499473 0.21875,0.625 a 0.58589857,0.58589857 0 0 1 0.125,0.0625 0.58589857,0.58589857 0 0 1 0.125,0.09375 l 5.25,2.21875 c 0.136378,0.05769 0.222914,0.04268 0.3125,0 0.08959,-0.04268 0.205135,-0.132566 0.28125,-0.3125 l 1.1875,-2.8125 c 0.18135,-0.451056 0.645358,-0.704244 1.125,-0.625 l 0,-0.03125 c 1.408622,0.200752 2.89216,0.21057 4.34375,0.03125 0.01044,-0.0013 0.02081,0.0013 0.03125,0 0.472005,-0.05199 0.927532,0.243869 1.09375,0.6875 0.003,0.0081 -0.0028,0.02305 0,0.03125 0.0022,0.0064 0.02916,-0.0064 0.03125,0 l 1.09375,2.78125 c 0.124282,0.306518 0.34883,0.393228 0.625,0.28125 a 0.58589857,0.58589857 0 0 1 0.03125,0 0.58589857,0.58589857 0 0 1 0.03125,-0.03125 l 0.0625,0 A 0.58589857,0.58589857 0 0 1 29.09375,43 l 5.375,-2.15625 c 0.248028,-0.100567 0.350598,-0.331075 0.21875,-0.65625 a 0.58589857,0.58589857 0 0 1 0,-0.03125 L 34.65625,40.125 A 0.58589857,0.58589857 0 0 1 34.625,40 l -1.0625,-2.65625 a 0.58589857,0.58589857 0 0 1 -0.03125,0 c -0.165011,-0.435067 7.2e-4,-0.942298 0.375,-1.21875 1.173373,-0.883881 2.179388,-1.890575 3.0625,-3.03125 0.27834,-0.398685 0.802306,-0.526888 1.25,-0.34375 l 0.03125,0 2.78125,1.1875 c 0.337099,0.142597 0.499474,0.07799 0.625,-0.21875 a 0.58589857,0.58589857 0 0 1 0.0625,-0.125 A 0.58589857,0.58589857 0 0 1 41.75,33.5625 l 0.03125,-0.03125 a 0.58589857,0.58589857 0 0 1 0.03125,-0.0625 l 2.21875,-5.25 c 0.05769,-0.136379 0.04268,-0.222914 0,-0.3125 -0.04268,-0.08959 -0.132566,-0.205136 -0.3125,-0.28125 l -2.8125,-1.1875 c -0.451056,-0.18135 -0.704244,-0.645358 -0.625,-1.125 l -0.03125,0 c 0.200752,-1.408622 0.21057,-2.89216 0.03125,-4.34375 -0.0015,-0.01034 0.0011,-0.02092 0,-0.03125 -0.05199,-0.472005 0.243869,-0.927532 0.6875,-1.09375 l 0.03125,0 0,-0.03125 2.78125,-1.09375 c 0.306518,-0.124282 0.393228,-0.34883 0.28125,-0.625 a 0.58589857,0.58589857 0 0 1 0,-0.03125 l -0.03125,-0.03125 a 0.58589857,0.58589857 0 0 1 0,-0.03125 l 0,-0.03125 A 0.58589857,0.58589857 0 0 1 44,17.90625 l -2.15625,-5.375 C 41.743183,12.283223 41.512674,12.180652 41.1875,12.3125 a 0.58589857,0.58589857 0 0 1 -0.03125,0 L 41,12.375 38.34375,13.4375 a 0.58589857,0.58589857 0 0 1 0,0.03125 c -0.435067,0.165011 -0.942298,-7.2e-4 -1.21875,-0.375 -0.883882,-1.173371 -1.890577,-2.179389 -3.03125,-3.0625 C 33.695066,9.75291 33.566862,9.2289435 33.75,8.78125 l 0,-0.03125 1.1875,-2.78125 c 0.142596,-0.3370972 0.07799,-0.4994745 -0.21875,-0.625 A 0.58589857,0.58589857 0 0 1 34.59375,5.28125 0.58589857,0.58589857 0 0 1 34.5625,5.25 L 34.53125,5.21875 A 0.58589857,0.58589857 0 0 1 34.46875,5.1875 l -5.25,-2.21875 c -0.13638,-0.057691 -0.222914,-0.042674 -0.3125,0 -0.08959,0.042674 -0.205136,0.1325669 -0.28125,0.3125 l -1.1875,2.8125 c -0.18135,0.4510555 -0.645356,0.7042451 -1.125,0.625 l 0,0.03125 C 24.903877,6.5492484 23.42034,6.5394304 21.96875,6.71875 21.95841,6.72021 21.94783,6.71761 21.9375,6.71875 21.465495,6.77074 21.009968,6.474881 20.84375,6.03125 20.838358,6.0168582 20.817276,6.0147294 20.8125,6 L 19.71875,3.21875 C 19.594467,2.9122321 19.36992,2.8255215 19.09375,2.9375 a 0.58589857,0.58589857 0 0 1 -0.03125,0 z m 5.03125,12.5 C 27.036457,15.482032 29.82811,17.266036 31,20.15625 32.562013,24.00862 30.695303,28.438319 26.84375,30 22.99138,31.562013 18.561681,29.695303 17,25.84375 15.437987,21.99138 17.304697,17.561681 21.15625,16 c 0.968461,-0.392679 1.964139,-0.57723 2.9375,-0.5625 z' transform='matrix(0.9576328,0,0,0.9576328,1.0168128,1.2074984)' id='3' opacity='0.8' fill='none' display='block' stroke='url(#I)' stroke-linejoin='round'/>
 <path d='m 19.142224,11.252389 c -6.606139,2.678578 -9.8014659,10.232302 -7.122887,16.83844 2.678578,6.606138 10.232301,9.801465 16.838439,7.122887 6.606139,-2.678579 9.801466,-10.232303 7.122887,-16.83844 C 33.302085,11.769137 25.748362,8.5738109 19.142224,11.252389 z' id='4' fill='none' stroke='url(#F)' stroke-width='0.958'/>
 <path d='m 24,10 c -7.168321,-5e-7 -13,5.83168 -13,13 0,7.16832 5.831679,13 13,13 7.168319,-10e-7 13,-5.83168 13,-13 0,-7.16832 -5.831681,-13 -13,-13 z m 0,6 c 3.864,0 7,3.136 7,7 0,3.864 -3.136,7 -7,7 -3.864,0 -7,-3.136 -7,-7 0,-3.864 3.136,-7 7,-7 z' transform='matrix(0.8874565,-0.3598353,0.3598353,0.8874565,-5.5751693,11.457599)' id='5' opacity='0.24' fill='#fff'/>
 <path d='m 18.84375,2.40625 -5.5,2.21875 -0.03125,0 c -0.528698,0.2289371 -0.761982,0.8371999 -0.53125,1.40625 a 0.45836943,0.45836943 0 0 1 0,0.03125 l 1.125,2.8125 A 0.45836943,0.45836943 0 0 1 13.75,9.40625 c -1.216491,0.916365 -2.274365,1.976796 -3.1875,3.15625 A 0.45836943,0.45836943 0 0 1 10,12.71875 L 7.1875,11.53125 C 6.8864361,11.403896 6.6309272,11.398747 6.375,11.5 6.1123516,11.603913 5.926742,11.792433 5.8125,12.0625 a 0.45836943,0.45836943 0 0 1 -0.03125,0 l -2.34375,5.5 0.03125,0 c -0.2334857,0.551958 0.00989,1.096768 0.59375,1.34375 l 2.8125,1.1875 a 0.45836943,0.45836943 0 0 1 0.28125,0.5 C 6.9463896,22.066287 6.9351212,23.611716 7.125,25.125 a 0.45836943,0.45836943 0 0 1 -0.3125,0.5 L 4.03125,26.75 4,26.75 c -0.5580433,0.240119 -0.8133848,0.833318 -0.59375,1.375 a 0.45836943,0.45836943 0 0 1 0,0.03125 l 2.21875,5.5 0,0.03125 c 0.2289372,0.528699 0.8371997,0.761982 1.40625,0.53125 a 0.45836943,0.45836943 0 0 1 0.03125,0 l 2.8125,-1.125 A 0.45836943,0.45836943 0 0 1 10.40625,33.25 c 0.916364,1.216492 1.976796,2.274365 3.15625,3.1875 A 0.45836943,0.45836943 0 0 1 13.71875,37 l -1.1875,2.8125 c -0.245014,0.579211 -0.02286,1.140602 0.53125,1.375 a 0.45836943,0.45836943 0 0 1 0,0.03125 l 5.5,2.34375 0,-0.03125 c 0.551955,0.233486 1.096767,-0.0099 1.34375,-0.59375 l 1.1875,-2.8125 a 0.45836943,0.45836943 0 0 1 0.5,-0.28125 c 1.472537,0.209861 3.017966,0.221129 4.53125,0.03125 a 0.45836943,0.45836943 0 0 1 0.5,0.3125 l 1.125,2.78125 0,0.03125 c 0.240119,0.558043 0.833318,0.813385 1.375,0.59375 a 0.45836943,0.45836943 0 0 1 0.03125,0 l 5.5,-2.21875 c 0.0075,-0.0031 0.02381,0.0032 0.03125,0 0.5287,-0.228938 0.761982,-0.8372 0.53125,-1.40625 a 0.45836943,0.45836943 0 0 1 0,-0.03125 l -1.125,-2.8125 A 0.45836943,0.45836943 0 0 1 34.25,36.59375 c 1.216492,-0.916362 2.274364,-1.976795 3.1875,-3.15625 A 0.45836943,0.45836943 0 0 1 38,33.28125 l 2.8125,1.1875 c 0.579211,0.245013 1.140603,0.02286 1.375,-0.53125 a 0.45836943,0.45836943 0 0 1 0.03125,0 l 2.34375,-5.5 -0.03125,0 c 0.233486,-0.551956 -0.0099,-1.096769 -0.59375,-1.34375 l -2.8125,-1.1875 a 0.45836943,0.45836943 0 0 1 -0.28125,-0.5 C 41.053611,23.933713 41.064879,22.388284 40.875,20.875 a 0.45836943,0.45836943 0 0 1 0.3125,-0.5 l 2.78125,-1.125 c 0.0079,-0.0032 0.02343,0.0034 0.03125,0 0.558043,-0.240119 0.813385,-0.833318 0.59375,-1.375 a 0.45836943,0.45836943 0 0 1 0,-0.03125 l -2.21875,-5.5 0,-0.03125 c -0.228938,-0.528699 -0.8372,-0.761982 -1.40625,-0.53125 a 0.45836943,0.45836943 0 0 1 -0.03125,0 l -2.8125,1.125 A 0.45836943,0.45836943 0 0 1 37.59375,12.75 C 36.677388,11.53351 35.616954,10.475635 34.4375,9.5625 A 0.45836943,0.45836943 0 0 1 34.28125,9 l 1.1875,-2.8125 C 35.713763,5.6082895 35.49161,5.0468967 34.9375,4.8125 a 0.45836943,0.45836943 0 0 1 0,-0.03125 l -5.5,-2.34375 0,0.03125 C 28.885542,2.2352645 28.34073,2.47864 28.09375,3.0625 l -1.1875,2.8125 a 0.45836943,0.45836943 0 0 1 -0.5,0.28125 C 24.933713,5.9463896 23.388284,5.9351212 21.875,6.125 a 0.45836943,0.45836943 0 0 1 -0.5,-0.3125 L 20.25,3.03125 20.25,3 C 20.009881,2.4419567 19.416682,2.1866152 18.875,2.40625 a 0.45836943,0.45836943 0 0 1 -0.03125,0 z m 5.25,13.625 c 2.712883,0.04105 5.293736,1.677045 6.375,4.34375 1.441792,3.55587 -0.289048,7.652432 -3.84375,9.09375 -3.55587,1.441792 -7.652432,-0.289048 -9.09375,-3.84375 -1.441792,-3.55587 0.289048,-7.652432 3.84375,-9.09375 0.890508,-0.361072 1.817564,-0.513638 2.71875,-0.5 z' transform='matrix(0.9576328,0,0,0.9576328,1.0168128,1.2074984)' id='6' fill='url(#E)' display='block'/>
</svg>
")

(define web::define-static-file web::settings-gear
  `(image/svg+xml) web::settings-gear-image-string)
