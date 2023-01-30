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
  (define-module (tegfs web-fileunknown-svg)
    :export (web::fileunknown.svg)
    :use-module ((tegfs web-define-static-file) :select (web::define-static-file)))))

(define web::fileunknown.svg::string
  "<?xml version='1.0' encoding='UTF-8' standalone='no'?>
<!--part of the matt icon theme by sixsixfive released under CC0 (https://creativecommons.org/publicdomain/zero/1.0/) on openclipart-->
<svg xmlns='http://www.w3.org/2000/svg' xmlns:xlink='http://www.w3.org/1999/xlink' viewBox='0 0 128 128'>
 <defs id='0'>
  <linearGradient id='7'>
   <stop id='I' stop-color='#eee'/>
   <stop offset='1' id='J' stop-color='#d2d2d2'/>
  </linearGradient>
  <linearGradient id='8'>
   <stop id='K'/>
   <stop offset='1' id='L' stop-opacity='0.536'/>
  </linearGradient>
  <linearGradient id='9'>
   <stop id='M'/>
   <stop offset='1' id='N' stop-color='#ddd' stop-opacity='0'/>
  </linearGradient>
  <filter id='A' x='-0.147' width='1.294' y='-0.145' height='1.29'>
   <feGaussianBlur stdDeviation='1.81881' id='O'/>
  </filter>
  <filter id='B'>
   <feGaussianBlur stdDeviation='2.58594' id='P'/>
  </filter>
  <radialGradient xlink:href='#7' id='C' gradientUnits='userSpaceOnUse' gradientTransform='matrix(2.0026052,-1.5970314,1.7773333,2.2286954,-279.23554,23.703826)' cx='81.79' cy='100.32' r='44.42'/>
  <radialGradient xlink:href='#8' id='D' gradientUnits='userSpaceOnUse' gradientTransform='matrix(1,0,0,0.97467889,0,0.43910718)' cx='89.51' cy='22.254' r='18.279'/>
  <linearGradient xlink:href='#9' id='E' gradientUnits='userSpaceOnUse' gradientTransform='matrix(0.96714879,0,0,0.96714879,0.45622123,-0.08680339)' x1='98.33' y1='14.793' x2='86.82' y2='25.1'/>
  <linearGradient xlink:href='#9' y2='25.1' x2='86.82' y1='14.793' x1='98.33' gradientTransform='matrix(0.96714879,0,0,0.96714879,1.5886796,3.2231964)' gradientUnits='userSpaceOnUse' id='F'/>
  <radialGradient xlink:href='#7' r='44.42' cy='100.32' cx='81.79' gradientTransform='matrix(2.0026052,-1.5970314,1.7773333,2.2286954,-278.10308,27.013826)' gradientUnits='userSpaceOnUse' id='G'/>
 </defs>
 <path d='m 20.09375,4.1185004 0,117.9999996 88.84375,0 0,-86.15625 -0.59375,-0.625 c 0.21045,0.144391 0.41688,0.302162 0.625,0.4375 L 78.09375,4.1497504 c 0.05654,0.079597 0.09993,0.1703021 0.15625,0.25 l -0.25,-0.28125 -57.90625,0 z' id='1' transform='matrix(0.96714879,0,0,0.97126149,1.5886796,2.6954315)' opacity='0.809' filter='url(#B)'/>
 <path d='m 21.022325,5.8526322 0,114.1235578 85.925125,0 0,-83.325913 -29.921165,-30.7976448 -56.00396,0 z' id='2' fill='url(#G)'/>
 <g id='3' fill='#555'>
  <path d='m 54.252249,35.043673 c -1.5e-5,1.032313 0.294913,2.046128 0.884785,3.041449 0.552975,0.737382 0.82947,1.751198 0.829486,3.04145 -1.6e-5,1.327235 -0.663604,2.39635 -1.990767,3.207347 -1.327191,0.811108 -2.893996,1.216634 -4.700422,1.21658 -1.585248,5.4e-5 -3.078322,-0.552936 -4.479226,-1.658973 -1.400913,-1.142791 -2.101368,-2.709596 -2.101365,-4.700422 -3e-6,-1.843241 0.479255,-3.483778 1.437776,-4.921619 0.995378,-1.437709 2.562184,-3.041381 4.700422,-4.81102 3.539129,-2.949211 8.700372,-4.423852 15.483744,-4.423927 6.414663,7.5e-5 11.502174,1.585314 15.262547,4.755722 3.797161,3.133681 5.695761,7.336407 5.695806,12.608191 -4.5e-5,1.585296 -0.294973,3.152102 -0.884785,4.700422 -0.553035,1.548425 -1.19819,2.875602 -1.935468,3.981534 -0.737363,1.106029 -1.769612,2.304175 -3.096749,3.594441 -1.327216,1.253489 -2.433197,2.230439 -3.317945,2.930851 -0.847955,0.663631 -2.027667,1.548415 -3.539141,2.654356 -0.995416,0.700494 -1.824901,1.364082 -2.488459,1.990767 -0.663619,0.62676 -1.382506,1.474678 -2.156665,2.543758 -0.737348,1.032284 -1.308771,2.304161 -1.714271,3.815637 -0.368687,1.511538 -0.553017,3.225808 -0.552991,5.142815 l 0,2.599057 c -2.6e-5,1.400932 -0.258088,2.396314 -0.774187,2.98615 -0.479283,0.589877 -1.548398,0.884805 -3.207347,0.884786 -1.622127,1.9e-5 -2.672808,-0.294909 -3.152048,-0.884786 -0.442411,-0.589836 -0.663607,-1.585218 -0.663589,-2.98615 l 0,-2.820253 c -1.8e-5,-1.548347 0.05528,-2.857091 0.165897,-3.926235 0.147446,-1.069085 0.442374,-2.451561 0.884786,-4.147432 0.479239,-1.695802 1.327157,-3.410072 2.543758,-5.142815 1.216557,-1.769529 2.76493,-3.520665 4.645123,-5.253413 2.101337,-1.843256 3.631276,-3.686557 4.589824,-5.529908 0.958485,-1.843251 1.437744,-4.110511 1.437776,-6.801788 -3.2e-5,-3.539081 -0.866384,-6.691125 -2.599057,-9.456143 -1.732733,-2.764885 -3.815663,-4.147361 -6.248797,-4.147431 -2.470046,7e-5 -4.589842,0.589926 -6.359394,1.76957 -1.73272,1.142916 -2.599072,2.525391 -2.599057,4.147432 m 2.488458,59.557114 c -1.6e-5,-1.769564 0.608273,-3.262638 1.82487,-4.479226 1.253426,-1.253435 2.764933,-1.880158 4.534525,-1.880169 1.73268,1.1e-5 3.225754,0.626734 4.479226,1.880169 1.253417,1.216588 1.880139,2.709662 1.880169,4.479226 -3e-5,1.695842 -0.626752,3.15205 -1.880169,4.368627 -1.253472,1.216576 -2.746546,1.824866 -4.479226,1.824866 -1.769592,0 -3.281099,-0.60829 -4.534525,-1.824866 -1.216597,-1.216577 -1.824886,-2.672785 -1.82487,-4.368627' id='H'/>
 </g>
 <path id='4' d='m 81.698039,28.309502 0.135161,-8.351 C 81.901124,13.359407 80.956142,8.4107397 77.785779,3.9471419 L 80.362343,3.9159389 107.86271,29.53084 c -4.06802,-2.645393 -9.621189,-0.791242 -16.861906,-1.025606 l -9.302765,-0.195732 z' transform='matrix(1.016085,0,0,1.112241,-3.7922382,2.9647584)' opacity='0.505' fill='url(#D)' filter='url(#A)'/>
 <path d='m 82.748319,29.121681 0.130721,-8.07666 c 0.06569,-6.382306 -2.691577,-10.83607 -5.75779,-15.1530328 l 29.86674,30.5881668 c -3.93438,-2.558489 -8.23966,-6.942507 -15.242513,-7.169171 l -8.997158,-0.189303 z' id='5' fill='#eee'/>
 <path id='6' d='m 82.748319,29.121681 0.130721,-8.07666 c 0.06569,-6.382306 -2.691577,-10.83607 -5.75779,-15.1530328 l 29.86674,30.5881668 c -3.93438,-2.558489 -8.23966,-6.942507 -15.242513,-7.169171 l -8.997158,-0.189303 z' fill='url(#F)'/>
</svg>")

(web::define-static-file
 web::fileunknown.svg
 `(image/svg+xml)
 web::fileunknown.svg::string)
