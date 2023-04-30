const openai = require("openai");

const configuration = new openai.Configuration({
  organization: "org-SEENiZrD0bycgv4C9vatl6W4",
  apiKey: "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",
});
const api = new openai.OpenAIApi(configuration);
const response = api.listEngines();

const logEngines = (response) => {
  const output = response.data.data.map((object) => object.id);
  console.log(output.sort((a, b) => a.localeCompare(b)));
};

response.then(logEngines, (e) => console.log("error:", e));
