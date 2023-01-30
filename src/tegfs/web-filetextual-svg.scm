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
  (define-module (tegfs web-filetextual-svg)
    :export (web::filetextual.svg)
    :use-module ((tegfs web-define-static-file) :select (web::define-static-file)))))

(define web::filetextual.svg::string
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
    id='svg3206'
    sodipodi:docname='fichier.svg'
    viewBox='0 0 113.61 153.03'
    sodipodi:version='0.32'
    version='1.0'
    inkscape:output_extension='org.inkscape.output.svg.inkscape'
    inkscape:version='0.46'
  >
  <sodipodi:namedview
      id='base'
      bordercolor='#666666'
      inkscape:pageshadow='2'
      inkscape:window-y='62'
      pagecolor='#ffffff'
      inkscape:window-height='774'
      inkscape:zoom='1.4142136'
      inkscape:window-x='177'
      showgrid='false'
      borderopacity='1.0'
      inkscape:current-layer='layer1'
      inkscape:cx='205.59158'
      inkscape:cy='109.16739'
      inkscape:window-width='1099'
      showborder='false'
      inkscape:pageopacity='0.0'
      inkscape:document-units='px'
  />
  <g
      id='layer1'
      inkscape:label='Calque 1'
      inkscape:groupmode='layer'
      transform='translate(-232.72 -456.83)'
    >
    <path
        id='path3216'
        sodipodi:nodetypes='ccccccccc'
        style='stroke-linejoin:round;fill-opacity:.99219;stroke:#000000;stroke-linecap:round;stroke-width:2;fill:#ffffff'
        d='m233.72 462.95l1.12 137.2c0.94 2.27-0.63 3.8 3.04 5.33l101.53 0.53c3.67 0.42 5.17-0.78 5.07-3.19l0.84-113.07c0.35-1.63-27.65-29.47-29.91-31.85l-76.11 0.27c-4.09-0.56-5.18 1.47-5.58 4.78z'
    />
    <path
        id='path3727'
        sodipodi:nodetypes='cc'
        style='stroke-linejoin:round;fill-opacity:.99219;stroke:#000000;stroke-linecap:round;fill:#ffffff'
        d='m234.59 481.6l96.85 0.42'
    />
    <path
        id='path3729'
        style='stroke-linejoin:round;fill-opacity:.99219;stroke:#000000;stroke-linecap:round;fill:#ffffff'
        d='m340.47 609.36z'
    />
    <path
        id='path3743'
        style='stroke-linejoin:round;fill-opacity:.99219;stroke:#000000;stroke-linecap:round;fill:#ffffff'
        d='m246.07 516.38l89.1-0.71'
    />
    <path
        id='path3745'
        style='stroke-linejoin:round;fill-opacity:.99219;stroke:#000000;stroke-linecap:round;fill:#ffffff'
        d='m245.37 529.81l88.38 0.71'
    />
    <path
        id='path3747'
        style='stroke-linejoin:round;fill-opacity:.99219;stroke:#000000;stroke-linecap:round;fill:#ffffff'
        d='m244.66 543.95l89.8 0.71'
    />
    <path
        id='path3749'
        style='stroke-linejoin:round;fill-opacity:.99219;stroke:#000000;stroke-linecap:round;fill:#ffffff'
        d='m245.37 558.09l88.38-1.41'
    />
    <path
        id='path3751'
        style='stroke-linejoin:round;fill-opacity:.99219;stroke:#000000;stroke-linecap:round;fill:#ffffff'
        d='m245.01 570.12l88.39 1.41'
    />
    <path
        id='text3739'
        style='stroke:#000000;stroke-width:1px;fill:#ffffff'
        d='m299.54 568.42c-7.26 5.6-11.81 8.84-13.66 9.72-2.78 1.28-5.73 1.93-8.87 1.93-4.89 0-8.92-1.68-12.11-5.02-3.14-3.34-4.71-7.74-4.71-13.19 0-3.45 0.77-6.43 2.31-8.95 2.11-3.5 5.77-6.79 10.96-9.87 5.25-3.09 13.94-6.85 26.08-11.27v-2.78c0-7.04-1.14-11.88-3.4-14.5-2.21-2.62-5.45-3.93-9.72-3.93-3.24 0-5.81 0.87-7.71 2.62-1.96 1.75-2.94 3.75-2.94 6.02l0.16 4.47c0 2.37-0.62 4.19-1.85 5.48-1.19 1.28-2.75 1.93-4.71 1.93-1.9 0-3.47-0.67-4.71-2.01-1.18-1.34-1.77-3.16-1.77-5.48 0-4.42 2.26-8.48 6.79-12.19 4.53-3.7 10.88-5.55 19.06-5.55 6.27 0 11.41 1.05 15.42 3.16 3.04 1.6 5.28 4.09 6.72 7.49 0.92 2.21 1.39 6.73 1.39 13.57v24c0 6.73 0.12 10.88 0.38 12.42 0.26 1.49 0.67 2.49 1.24 3.01 0.61 0.51 1.31 0.77 2.08 0.77 0.82 0 1.54-0.18 2.16-0.54 1.08-0.67 3.16-2.55 6.25-5.63v4.32c-5.76 7.71-11.27 11.57-16.51 11.57-2.52 0-4.53-0.88-6.02-2.62-1.49-1.75-2.26-4.74-2.31-8.95m0-5.02v-26.92c-7.77 3.08-12.78 5.27-15.05 6.56-4.06 2.26-6.97 4.62-8.72 7.09-1.74 2.47-2.62 5.17-2.62 8.1 0 3.71 1.11 6.79 3.32 9.26 2.21 2.42 4.76 3.63 7.64 3.63 3.91 0 9.05-2.57 15.43-7.72'
    />
    <path
        id='path3753'
        sodipodi:nodetypes='cc'
        style='stroke-linejoin:round;fill-opacity:.99219;stroke:#000000;stroke-linecap:round;fill:#ffffff'
        d='m244.31 501.53h90.51'
    />
    <path
        id='path3775'
        style='stroke-linejoin:round;fill-opacity:.99219;stroke:#000000;stroke-linecap:round;fill:#ffffff'
        d='m245.1 582.99l88.38 1.41'
    />
    <path
        id='path3777'
        sodipodi:nodetypes='cccc'
        style='stroke-linejoin:round;fill-opacity:.99219;stroke:#000000;stroke-linecap:round;stroke-width:2;fill:#ffffff'
        d='m315.1 457.83l-1.06 31.82 31.11-0.71-30.05-31.11z'
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
          >Fichier / file</dc:title
        >
        <dc:date
          >2010-03-22T22:49:32</dc:date
        >
        <dc:description
        />
        <dc:source
          >https://openclipart.org/detail/33301/fichier-/-file-by-lmproulx-33301</dc:source
        >
        <dc:creator
          >
          <cc:Agent
            >
            <dc:title
              >lmproulx</dc:title
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
              >black and white</rdf:li
            >
            <rdf:li
              >computer</rdf:li
            >
            <rdf:li
              >document</rdf:li
            >
            <rdf:li
              >icon</rdf:li
            >
            <rdf:li
              >line art</rdf:li
            >
            <rdf:li
              >outline</rdf:li
            >
            <rdf:li
              >paper</rdf:li
            >
            <rdf:li
              >text</rdf:li
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
>")

(web::define-static-file
 web::filetextual.svg
 `(image/svg+xml)
 web::filetextual.svg::string)
