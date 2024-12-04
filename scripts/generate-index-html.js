const fs = require("node:fs");
const dataFileContent = fs.readFileSync("../noc-data.json");
const data = JSON.parse(String(dataFileContent));

// const e1 = data[0];
// const template = `<a href="#">${e1[0]}: ${e1[1]}</a>`;
// console.log(template);

function linkFromTitle(string) {
  const [ch, eg] = string
    .split(" ")[1]
    .split(".")
    .map((s) =>
      parseInt(s).toLocaleString("en-US", {
        minimumIntegerDigits: 2,
        useGrouping: false,
      }),
    );
  return `Ch${ch}/Eg${eg}.html`;
}

const res = data
  .map(([title, description]) => {
    const link = linkFromTitle(title);
    return `<a href="${link}">${title}: ${description}</a>`;
  })
  .join("\n");

console.log(res);
