
const fs = require('fs');
const { JSDOM } = require('jsdom');
const domino = require('domino');
const input = fs.readFileSync('input.txt', 'utf8');
const content = fs.readFileSync('input.html', 'utf8').replace('{{code}}', input);

// Create a basic DOM environment using domino
const dom = new JSDOM(content, {
  pretendToBeVisual: true,
  beforeParse(window) {
    const { document } = window;
    // Polyfill Element for Prism.js
    global.Element = domino.impl.Element;
    // Polyfill other necessary objects and APIs as needed
    global.window = window;
    global.document = document;
    global.navigator = window.navigator;
    global.requestAnimationFrame = window.requestAnimationFrame;
    // Add any other browser-specific objects or APIs to the global object
  },
});

// const Prism = require('prismjs');
const Prism = require('./prism.js');
require('prismjs/components/')(); // Import necessary Prism.js components

// Create a new DOM element and set its innerHTML to the HTML content
const div = document.createElement('div');
div.innerHTML = content;

// Highlight the code within the DOM element
Prism.highlightAllUnder(div);

// Get the modified HTML after highlighting
const modifiedHtml = div.innerHTML;

// You can then do whatever you want with the modified HTML
console.log(modifiedHtml);

const template = fs.readFileSync("template.html", "utf8");
const processedData = template.replace('{{code}}', modifiedHtml);
fs.writeFileSync("output.html", processedData);

// Prism.highlightAll();

// console.log({dom});

// const text = dom.outerHTML;
// console.log({text});

// function parseHTMLElement(html) {
//   const { document } = new JSDOM(html).window;
//   return document.body.firstChild;
// }

// // let langs = Object.keys(Prism.languages);
// // for (let x of langs) {
// //     // console.log({langs});
// //     console.log({x});
// // }
// // return;
// // exit(0);

// // Read the template file
// fs.readFile('input.html', 'utf8', (err, data) => {
//   if (err) {
//     console.error(err);
//     return;
//   }

//   // Process the template file to include Prism.js code highlighting
//   const processedData = processData(data);

//   // Write the processed data to a new file
//   fs.writeFile('output.html', processedData, 'utf8', (err) => {
//     if (err) {
//       console.error(err);
//       return;
//     }

//     console.log('Static HTML file generated successfully!');
//   });
// });

// // // Function to process the template data and include Prism.js code highlighting
// // function processData(data) {
// //   // Replace placeholder code block in the template with Prism.js highlighted code
// //   const code = fs.readFileSync('input.txt', 'utf8');
// //   const highlightedCode = Prism.highlight(code, Prism.languages.bash, 'bash');
// //   const highlightedCode = Prism.highlightAll(code, Prism.languages.bash, 'bash');
// //   const processedData = data.replace('{{code}}', highlightedCode);

// //   return processedData;
// // }



// // Function to process the template data and include Prism.js code highlighting
// function processData(data) {
//   // Invoke Prism.highlightAll to highlight all code blocks in the document
//   console.log({data});
//   const elem = parseHTMLElement(data);
//   console.log({elem});
//   const processedData = Prism.highlightElement(elem);
//   console.log({processedData});
//   const result = elem.outerHTML;
//   console.log({result});
//   // const result = processedData();
//   // console.log({result});

//   return result;
// }
