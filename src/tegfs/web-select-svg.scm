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
  (define-module (tegfs web-select-svg)
    :export (web::select.svg)
    :use-module ((tegfs web-define-static-file) :select (web::define-static-file))
    )))



(define web::select.svg::string
  "<?xml version='1.0' encoding='UTF-8' standalone='no'?>
<!-- Created with Inkscape (http://www.inkscape.org/) -->
<svg
    xmlns:inkscape='http://www.inkscape.org/namespaces/inkscape'
    xmlns:rdf='http://www.w3.org/1999/02/22-rdf-syntax-ns#'
    xmlns='http://www.w3.org/2000/svg'
    xmlns:cc='http://creativecommons.org/ns#'
    xmlns:dc='http://purl.org/dc/elements/1.1/'
    xmlns:sodipodi='http://sodipodi.sourceforge.net/DTD/sodipodi-0.dtd'
    xmlns:svg='http://www.w3.org/2000/svg'
    xmlns:ns1='http://sozi.baierouge.fr'
    xmlns:xlink='http://www.w3.org/1999/xlink'
    id='svg2'
    sodipodi:docname='jetxee_check_sign_and_cross_sign_yes.svg'
    inkscape:export-filename='/home/sergey/pix/shared/check.png'
    viewBox='0 0 43.497 44.277'
    sodipodi:version='0.32'
    inkscape:export-xdpi='90'
    version='1.0'
    inkscape:output_extension='org.inkscape.output.svg.inkscape'
    inkscape:export-ydpi='90'
    inkscape:version='0.45.1+0.46pre1+devel'
    sodipodi:docbase='/home/sergey/pix/shared'
  >
  <sodipodi:namedview
      id='base'
      bordercolor='#666666'
      inkscape:pageshadow='2'
      guidetolerance='10.0'
      pagecolor='#ffffff'
      gridtolerance='10.0'
      inkscape:zoom='4.2395833'
      objecttolerance='10.0'
      borderopacity='1.0'
      inkscape:current-layer='svg2'
      inkscape:cx='29.780401'
      inkscape:cy='17.464263'
      inkscape:window-y='87'
      inkscape:window-x='66'
      inkscape:window-height='664'
      showgrid='false'
      inkscape:pageopacity='0.0'
      inkscape:window-width='948'
  />
  <g
      id='g2392'
    >
    <path
        id='rect2160'
        style='fill:#1ab188'
        d='m0 5.5893v38.688h38.688v-38.688l-38.688 0.0003zm6.3125 5.5627h26.062l0.001 27.594h-26.062l-0.0005-27.594z'
    />
    <path
        id='path2170'
        style='stroke:#d7d7d7;stroke-width:7;fill:none'
        d='m11.789 16.766l5.859 15.84 16.349-23.106'
    />
  </g
  >
  <metadata
    >
    <rdf:RDF
      >
      <cc:Work
        >
        <dc:format
          >image/svg+xml</dc:format
        >
        <dc:type
            rdf:resource='http://purl.org/dc/dcmitype/StillImage'
        />
        <cc:license
            rdf:resource='http://creativecommons.org/licenses/publicdomain/'
        />
        <dc:publisher
          >
          <cc:Agent
              rdf:about='http://openclipart.org/'
            >
            <dc:title
              >Openclipart</dc:title
            >
          </cc:Agent
          >
        </dc:publisher
        >
        <dc:title
          >check sign and cross sign 3</dc:title
        >
        <dc:date
          >2006-12-26T00:00:00</dc:date
        >
        <dc:description
        />
        <dc:source
          >https://openclipart.org/detail/15820/-by--15820</dc:source
        >
        <dc:creator
          >
          <cc:Agent
            >
            <dc:title
              >jetxee</dc:title
            >
          </cc:Agent
          >
        </dc:creator
        >
      </cc:Work
      >
      <cc:License
          rdf:about='http://creativecommons.org/licenses/publicdomain/'
        >
        <cc:permits
            rdf:resource='http://creativecommons.org/ns#Reproduction'
        />
        <cc:permits
            rdf:resource='http://creativecommons.org/ns#Distribution'
        />
        <cc:permits
            rdf:resource='http://creativecommons.org/ns#DerivativeWorks'
        />
      </cc:License
      >
    </rdf:RDF
    >
  </metadata
  >
</svg
>
")

(web::define-static-file
 web::select.svg
 `(image/svg+xml)
 web::select.svg::string)
