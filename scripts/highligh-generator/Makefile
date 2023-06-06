
output.html: template.html node_modules/ generate.js input.html input.txt
	npm run generate

node_modules/: package.json
	npm install
	touch "$@"
