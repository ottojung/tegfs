
const fs = require('fs');
const Prism = require('prismjs');
require('prismjs/components/')(); // Import necessary Prism.js components

// Read the template file
fs.readFile('template.html', 'utf8', (err, data) => {
  if (err) {
    console.error(err);
    return;
  }

  // Process the template file to include Prism.js code highlighting
  const processedData = processData(data);

  // Write the processed data to a new file
  fs.writeFile('output.html', processedData, 'utf8', (err) => {
    if (err) {
      console.error(err);
      return;
    }

    console.log('Static HTML file generated successfully!');
  });
});

// Function to process the template data and include Prism.js code highlighting
function processData(data) {
  // Replace placeholder code block in the template with Prism.js highlighted code
  const code = fs.readFileSync('input.txt', 'utf8');
  const highlightedCode = Prism.highlight(code, Prism.languages.javascript, 'javascript');
  const processedData = data.replace('{{code}}', highlightedCode);

  return processedData;
}
