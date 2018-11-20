var express = require('express');
var graphqlHTTP = require('express-graphql');
var { buildSchema } = require('graphql');
var animals = require('./animals.json');

for(var i = 0; i < animals.length; i++) {
  animals[i].id = i;
}

// Construct a schema, using GraphQL schema language
var schema = buildSchema(`
  type Query {
    animals: [Animal]!
    animal(id: Int!): Animal
  }

  type Mutation {
    createAnimal()
  }

  type Animal {
    id: Int!
    species: [String]!
    mask: [String]
    link: String!
    popularity: Int!
    taxonomy: Taxonomy!
    distribution: String
    habitat: String
    behavior: String
    characteristic: String
    reproduction: String
    diet: String
    socialStructure: String
  }

  type Taxonomy {
    kingdom: String
    phylum: String
    class: String
    superorder: String
    order: String
    suborder: String
    infraorder: String
    superfamily: String
    family: String
    subfamily: String
    without_rank: String
    tribe: String
    genus: String
    species: String
  }
`);

// The root provides a resolver function for each API endpoint
var root = {
  animals: () => {
    return animals;
  },

  animal: (args) => {
    return animals[args.id];
  },
};

var app = express();
app.use('/graphql', graphqlHTTP({
  schema: schema,
  rootValue: root,
  graphiql: true,
}));
app.listen(4000);
console.log('Running a GraphQL API server at localhost:4000/graphql');
