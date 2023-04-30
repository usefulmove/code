const { Configuration, OpenAIApi } = require("openai");

const configuration = new Configuration({
  organization: "org-SEENiZrD0bycgv4C9vatl6W4",
  apiKey: "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",
});
const openai = new OpenAIApi(configuration);

/* list engines */
//const engines_response = openai.listEngines();

//const logEngines = (engines_response) => {
//  const output = engines_response.data.data.map((object) => object.id);
//  console.log(output.sort((a, b) => a.localeCompare(b)));
//};

//engines_response.then(logEngines, (e) => console.log("error:", e));

/* create completion */
const response = openai.createCompletion({
  model: "text-davinci-003",
  prompt: "You: How many planets are there in our solar system? chatbot: There are nine planets? You: I didn't think Pluto was a planet.",
  temperature: 0.2,
//  max_tokens: 150,
//  top_p: 1.0,
//  frequency_penalty: 0.5,
//  presenec_penalty: 0.0,
//  stop: ["You:"],
});

const logReponse = (response) => {
  const output = response.data.choices[0].text;
  console.log(output);
};

response.then(
  logReponse,
  (e) => console.log("error:", e)
);
