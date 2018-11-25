import React, { Component } from 'react';
import Button from 'react-bootstrap/lib/Button';
import Form from 'react-bootstrap/lib/Form'
import FormControl from 'react-bootstrap/lib/FormControl'
import FormGroup from 'react-bootstrap/lib/FormGroup'
import Col from 'react-bootstrap/lib/Col'

import './App.css';
import animals from './animals.json';
import { uniformDist } from './Random.js';

import levenshtein from 'js-levenshtein';

import ApolloClient from 'apollo';
import gql from 'graphql-tag';

const client = new ApolloClient()

const minimum = (numArray) => Math.min.apply(null, numArray);

const shownSize = (sentence) => {
  if(sentence.masked === "shown")
    return sentence.text.length
  else
    return 0
};

const size = (sentences) => sentences.reduce((acc,sentence) => acc + shownSize(sentence),0)

const numShown = (maskedSentences) => maskedSentences.filter(s => s.masked === "shown").length

const removeSentences = (percent,sentences) => {
  var reduced = sentences.map((sentence,i) => {return {id:i, masked:"shown", text:sentence}})
  let totalSize = size(reduced)
  while(size(reduced) > totalSize * percent && numShown(reduced) > 1) {
    let red = reduced.filter(s => s.masked === "shown")
    let r = Math.floor(uniformDist(0,red.length))
    reduced[red[r].id].masked = "hidden"
  }
  return reduced
};

const revealRandom = (maskedSentences) => {
  let hidden = maskedSentences.filter(s => s.masked === "hidden")
  if(hidden.length > 0) {
    let r = Math.floor(uniformDist(0,hidden.length))
    maskedSentences[hidden[r].id].masked = "shown"
  }
  return maskedSentences
}

const revealNth = (n) => (maskedSentences) => {
  maskedSentences[n].masked = "shown"
  return maskedSentences
}

const maskText = (animal) => (text) => {
  
  let masks = animal.species.concat(animal.species.map(s => s.toLowerCase()),
                                    animal.hasOwnProperty('mask') ? animal.mask.concat(animal.mask.map(s => s.toLowerCase())) : [])
  return text.replace(new RegExp(masks.reverse().join("|"),'g'),"...")
};

const printMaskedSentence = (animal,onClick) => (maskedSentence,i) => {
  if(maskedSentence.masked === "shown")
    return maskText(animal)(maskedSentence.text + ". ")
  else
    return <a key={i} onClick={() => onClick(i)}>[...] </a>;
};

const printMaskedSentences = (animal,onClick,maskedSentences) => maskedSentences.map(printMaskedSentence(animal,onClick))

const splitIntoSentences = (text) => text.split(/\.|\n/).map(s => s.trim()).filter(s => s.length !== 0)

// const addOrZero = (obj1,obj2,attr) => obj1.taxonomy[attr] === obj2.taxonomy[attr] ? 1 : 0
  
// const similarTaxonomy = (a1,a2) =>
//    1 * addOrZero(a1,a2,'kingdom') +
//    2 * addOrZero(a1,a2,'phylum') +
//    3 * addOrZero(a1,a2,'class') +
//    4 * addOrZero(a1,a2,'superorder') +
//    5 * addOrZero(a1,a2,'order') +
//    6 * addOrZero(a1,a2,'suborder') +
//    7 * addOrZero(a1,a2,'infraorder') +
//    8 * addOrZero(a1,a2,'superfamily') +
//    9 * addOrZero(a1,a2,'family') +
//   10 * addOrZero(a1,a2,'subfamily') +
//   11 * addOrZero(a1,a2,'without_rank') +
//   11 * addOrZero(a1,a2,'tribe') +
//   12 * addOrZero(a1,a2,'genus') +
//   13 * addOrZero(a1,a2,'species')
  
const animalAttributes = [
  {attribute: "Verbreitung",    text:  (animal) => animal.distribution },
  {attribute: "Habitat",        text:  (animal) => animal.habitat },
  {attribute: "Ernährung",      text:  (animal) => animal.diet },
  {attribute: "Sozialstruktur", text:  (animal) => animal.socialStructure },
  {attribute: "Fortpflanzung",  text:  (animal) => animal.reproduction},
  {attribute: "Verhalten",      text:  (animal) => animal.behavior},
  {attribute: "Merkmale",       text:  (animal) => animal.characteristic},
  {attribute: "Bild",           image: (animal) => animal.image}
];

class RevealImage extends Component {
  constructor(props) {
    super(props);
    this.state = {
      revealed: false
    }
  }

  reveal() {
    this.setState({revealed:true})
  }

  render() {
    return (this.state.revealed || this.props.revealed)
      ? <img className="animal" src={this.props.image} alt="Abbildung des Tiers"/>
      : <Button onClick={() => this.reveal()}>Bild anzeigen</Button>
  }
}

class AnimalAttribute extends Component {
  constructor(props) {
    super(props)
    if(this.props.attr.hasOwnProperty('text'))
      this.state = {
        text: removeSentences(this.props.difficulty,splitIntoSentences(this.props.attr.text(this.props.animal)))
      };
    else
      this.state = {
        image: this.props.attr.image(this.props.animal)
      };
  }
  
  revealRandom() {
    this.setState({attr: revealRandom(this.state.text)})
  }
  
  revealNth(n) {
    this.setState({attr: revealNth(n)(this.state.text)})
  }

  render() {
    return (
      <div>
        <h3>{this.props.attr.attribute}</h3>
        { this.props.attr.hasOwnProperty('text')
          ? <p lang="de">{printMaskedSentences(this.props.animal,(n) => this.revealNth(n),this.state.text)}</p>
          : <RevealImage image={this.state.image} revealed={this.props.resolved} />
        }
      </div>
    );
  }
}

class Guesser extends Component {
  constructor(props) {
    super(props);
    this.state = {
      answer: "",
      resolved: false
    };
  }

  handleChange(e) {
    this.setState({answer: e.target.value})
    this.state.answer = e.target.value
    if(this.answered() && this.answeredCorrectly() === 0) {
      this.resolve();
    }
  }

  answered() {
    return this.state.answer !== ""
  }

  answeredCorrectly() {
    return minimum(this.props.animal.species.map(s => levenshtein(s.toLowerCase(),this.state.answer.toLowerCase())))
  }

  resolve() {
    this.setState({resolved:true})
    this.props.onResolve();
  }

  getValidationState() {
    if(this.answered()) {
      const correct = this.answeredCorrectly();
      if (correct === 0) return 'success';
      else if (correct <= 2) return 'warning';
      else if (correct >= 3) return 'error';
    } else {
      return null;
    }
  }

  render() {
    if(this.state.resolved) {
      return (
        <div className="guesser">
          <h2>Die Lösung ist {this.props.animal.species[0]}</h2>
          <br/> 
          Quelle: <a href={this.props.animal.link}>Wikipedia</a>
        </div>
      );
    } else if(this.answered() && this.answeredCorrectly() === 0) {
      return (
        <div className="guesser">
          <h2>Korrekt. Gratulation</h2>
          <br/>
          Quelle: <a href={this.props.animal.link}>Wikipedia</a>
        </div>
      );
    } else {
      return (
        <div className="guesser">
          <Form horizontal>
            <FormGroup
              controlId="formBasicText"
              validationState={this.getValidationState()}
            >
              <Col >
                <FormControl
                  type="text"
                  value={this.state.answer}
                  placeholder="Name des Tiers"
                  onChange={(s) => this.handleChange(s)}
                />
              </Col>
              <Col>
                <Button onClick={() => this.resolve()}>Auflösen</Button>
              </Col>
            </FormGroup>
          </Form>
        </div>
      );
    }
  }
}

class GuessAnimal extends Component {
  constructor(props) {
    super(props);
    this.animal = this.props.animal
    this.state = {
      resolved: false
    }
  }

  resolve() {
    this.setState({resolved: true})
  }
  
  render() {
    return (
      <div>
        <h1>Errate das Tier</h1>
        {animalAttributes.map(
          (attr,i) => <AnimalAttribute
            key={i.toString()}
            animal={this.animal}
            attr={animalAttributes[i]}
            difficulty={this.props.difficulty}
            resolved={this.state.resolved}
            />)}
        <Guesser animal={this.animal} onResolve={() => this.resolve()} />
      </div>
    );
  }
}


// class GameSelector extends Component {
//   constructor(props) {
//     super(props);
//     this.difficulties = [
//       { label: "Einfach", difficulty: 1 },
//       { label: "Mittel", difficulty: 0.75 },
//       { label: "Schwer", difficulty: 0.5 },
//       { label: "Sehr Schwer", difficulty: 0.25 },
//       { label: "Extrem", difficulty: 0.01 }
//     ];
//     this.state = {
//       animal:null
//     }
//   }

//   setDifficulty(difficulty) {
//     this.props.onSelection(this.state.animal,difficulty)
//   }

//   setAnimal(animal) {
//     this.setState({animal:animal})
//   }
  
//   selectRandomAnimal() {
//     let i = Math.floor(Math.random() * (animals.length-1))
//     this.setAnimal(animals[i])
//   }
  
//   render() {
//     if(!this.state.animal)
//       return (
//         <div>
//           <h1>Wähle ein Tier</h1>
//           <p><Button onClick={() => this.selectRandomAnimal()}>Zufällig</Button></p>
//           {animals.map((animal,i) => <Button key={i.toString()} onClick={() => this.setAnimal(animal)}>{i+1}</Button>)}
//         </div>
//       );

//     return (
//       <div>
//         <h1>Wähle eine Schwierigkeitsstuffe</h1>
//         {this.difficulties.map((d,i) =>
//          <Button key={i.toString()} onClick={() => this.setDifficulty(d.difficulty)}>{d.label}</Button>)}
//       </div>
//     );
//   }
// }

// class App extends Component {
//   constructor() {
//     super();
//     this.state = {
//       animal: null,
//       difficulty: null
//     }
//   }
  
//   selectAnimal(animal,difficulty) {
//     this.setState({animal:animal, difficulty:difficulty})
//   }

//   render() {
//     if(! this.state.animal)
//       return <GameSelector onSelection={(animal,difficulty) => this.selectAnimal(animal,difficulty)}/>;
//     else
//       return <GuessAnimal animal={this.state.animal} difficulty={this.state.difficulty}/>;
//   }
// }

class App extends Component {
  constructor() {
    super();
    this.state = {
      animal: null,
      animals: null,
    }
    var that = this;

    client.query({
      query: gql`
        query {
          animals {
            id
            popularity
          }
        }
      `
    }).then(data => that.setState({animals: data}))
      .catch(error => console.error(error));
  }

  render() {
    return animals;
    // return <GuessAnimal animal={this.state.animal} difficulty={this.state.difficulty}/>;
  }
}


export default App;
