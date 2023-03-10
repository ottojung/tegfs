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
  (define-module (tegfs web-search)
    :export (web::search.svg)
    :use-module ((tegfs web-define-static-file) :select (web::define-static-file))
    )))



(define web::search.svg-string "<?xml version='1.0' encoding='UTF-8' standalone='no'?>
<!-- Generator: Adobe Illustrator 9.0, SVG Export Plug-In  -->
<svg
    xmlns:inkscape='http://www.inkscape.org/namespaces/inkscape'
    xmlns:rdf='http://www.w3.org/1999/02/22-rdf-syntax-ns#'
    xmlns='http://www.w3.org/2000/svg'
    xmlns:cc='http://web.resource.org/cc/'
    xmlns:dc='http://purl.org/dc/elements/1.1/'
    xmlns:sodipodi='http://inkscape.sourceforge.net/DTD/sodipodi-0.dtd'
    xmlns:svg='http://www.w3.org/2000/svg'
    xmlns:ns1='http://sozi.baierouge.fr'
    xmlns:xlink='http://www.w3.org/1999/xlink'
    id='svg3467'
    space='preserve'
    sodipodi:docname='Lupa.svg'
    viewBox='0 0 491.237793 452.9882813'
    sodipodi:version='0.32'
    inkscape:version='0.42'
  >
  <sodipodi:namedview
      id='base'
      bordercolor='#666666'
      inkscape:pageshadow='2'
      inkscape:window-width='717'
      pagecolor='#ffffff'
      inkscape:zoom='0.60575518'
      inkscape:window-x='110'
      borderopacity='1.0'
      inkscape:current-layer='svg3467'
      inkscape:cx='307.02362'
      inkscape:cy='283.11768'
      inkscape:window-y='145'
      inkscape:window-height='510'
      inkscape:pageopacity='0.0'
  />
  <g
      id='Vrstva_x0020_1'
      style='stroke:#000000;fill:#1ab188'
    >
    <g
        id='g3470'
        style='clip-rule:evenodd;fill-rule:evenodd'
      >
      <path
          id='path3472'
          d='m328.09 256.78c-5.5918 8.1719-13.28 17.08-22.191 25.297-9.6855 8.9316-20.244 16.551-27.434 20.464l163.13 150.45 49.649-45.783-163.16-150.43z'
      />
      <path
          id='path3474'
          d='m283.83 45.058c-65.176-60.078-169.79-60.078-234.97 0-65.151 60.101-65.151 156.57 0 216.67 65.175 60.101 169.79 60.101 234.97 0 65.176-60.101 65.176-156.57 0-216.67zm-34.2 31.535c-46.204-42.607-120.39-42.607-166.57 0-46.205 42.583-46.205 110.99 0 153.6 46.18 42.606 120.37 42.606 166.57 0 46.205-42.607 46.205-111.02 0-153.6z'
      />
    </g
    >
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
          >hand-glass stylized mar 03r</dc:title
        >
        <dc:date
          >2010-10-12T22:33:15</dc:date
        >
        <dc:description
          >Originally uploaded for OCAL 0.18 by Martin Kozak</dc:description
        >
        <dc:source
          >https://openclipart.org/detail/90469/hand-glass-stylized-mar-03r-by-anonymous</dc:source
        >
        <dc:creator
          >
          <cc:Agent
            >
            <dc:title
              >Anonymous</dc:title
            >
          </cc:Agent
          >
        </dc:creator
        >
        <dc:subject
          >
          <rdf:Bag
            >
            <rdf:li
              >fix</rdf:li
            >
            <rdf:li
              >keyword</rdf:li
            >
            <rdf:li
              >librarian</rdf:li
            >
            <rdf:li
              >tag</rdf:li
            >
          </rdf:Bag
          >
        </dc:subject
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
 web::search.svg
 `(image/svg+xml) web::search.svg-string)

