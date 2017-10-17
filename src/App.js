import React, { Component } from 'react';
import './App.css';
import animals from './animals.json';
import noCheckCircle from './no_check_circle.svg';
import yesCheckCircle from './yes_check_circle.svg';
import emptyCheckCircle from './empty_check_circle.svg';

import levenshtein from 'js-levenshtein';

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
    let r = Math.floor(Math.random() * red.length)
    reduced[red[r].id].masked = "hidden"
  }
  return reduced
};

const revealRandom = (maskedSentences) => {
  let hidden = maskedSentences.filter(s => s.masked === "hidden")
  if(hidden.length > 0) {
    let r = Math.floor(Math.random() * hidden.length)
    maskedSentences[hidden[r].id].masked = "shown"
  }
  return maskedSentences
}

const revealNth = (n) => (maskedSentences) => {
  maskedSentences[n].masked = "shown"
  return maskedSentences
}

const completelyRevealed = (maskedSentences) => maskedSentences.every(s => s.masked === "shown")

const printMaskedSentence = (animal,onClick) => (maskedSentence,i) => {
  if(maskedSentence.masked === "shown")
    return maskSpecies(animal)(maskedSentence.text + ". ")
  else
      return <a onClick={() => onClick(i)}>[...] </a>;
};

const printMaskedSentences = (animal,onClick,maskedSentences) => maskedSentences.map(printMaskedSentence(animal,onClick))

const splitIntoSentences = (text) => text.split(/\.|\n/).map(s => s.trim()).filter(s => s.length !== 0)

const maskSpecies = (animal) => (text) => {
  let species = animal.species.concat(animal.species.map(s => s.toLowerCase()))
  return species.reduce((t,s) => t.replace(s,"..."),text)
};
  
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

function shuffle(array) {
  var currentIndex = array.length, temporaryValue, randomIndex;
  while (0 !== currentIndex) {
    randomIndex = Math.floor(Math.random() * currentIndex);
    currentIndex -= 1;
    temporaryValue = array[currentIndex];
    array[currentIndex] = array[randomIndex];
    array[randomIndex] = temporaryValue;
  }
  return array;
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
          ?
          <div>
            <p lang="de">{printMaskedSentences(this.props.animal,(n) => this.revealNth(n),this.state.text)}</p>
            <button disabled={completelyRevealed(this.state.text)} onClick={() => this.revealRandom()}>Zufälliger Tip</button>
          </div>
          :
          <img className="animal" src={this.state.attr.image} alt="Abbildung des Tiers"/>
        }
      </div>
    );
  }
}

class Guesser extends Component {
  constructor(props) {
    super(props);
    this.state = {
      answer: null,
      resolved: false
    };
  }

  answer() {
    var name = document.getElementById('guess').value
    document.getElementById('guess').value = ""
    this.setState({answer: name})
  }

  answered() {
    return this.state.answer !== null
  }

  answeredCorrectly() {
    return minimum(this.props.animal.species.map(s => levenshtein(s.toLowerCase(),this.state.answer.toLowerCase())))
  }

  resolve() {
    this.setState({resolved:true})
  }

  render() {
    if(this.state.resolved) {
      return (<p>Die Lösung ist {this.props.animal.species[0]}</p>);
    } else if(this.answered() && this.answeredCorrectly() === 0) {
      return (
        <p>Korrekt. Gratulation</p>
      );
    } else {
      return (
        <div>
          {! this.props.furtherHints && <button onClick={() => this.props.onHint()}>Nächste Kategorie</button>}
          <p>
            Name des Tiers: <input id="guess" type="text"></input><button onClick={() => this.answer()}>Tip abgeben</button>
          </p>
          {this.answered() && this.answeredCorrectly() >= 3 && <p>Nicht korrekt, rate weiter</p>}
          {this.answered() && this.answeredCorrectly() <= 2 && <p>Fast richtig, {this.answeredCorrectly()} Tippfehler</p>}
          <button onClick={() => this.resolve()}>Auflösen</button>
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
      lastAttr: 1,
      answer: null
    };
  }
  
  nextCategory() {
    this.setState({lastAttr:this.state.lastAttr+1})
  }
  
  render() {
    return (
      <div>
        <h1>Errate das Tier</h1>
        {animalAttributes.slice(0,this.state.lastAttr).map((attr,i) => <AnimalAttribute key={i.toString()} animal={this.animal} attr={animalAttributes[i]} difficulty={this.props.difficulty}/>)}
        <Guesser animal={this.animal} onHint={() => this.nextCategory()} furtherHints={this.state.lastAttr >= animalAttributes.length}/>
      </div>
    );
  }
}

class AnimalTrivia extends Component {
  constructor(props) {
    super(props);
    this.animal = props.animal
    var animalList = [this.animal]
    for(var i = 1; i <= 3; i++) {
      let r = Math.floor(Math.random() * animals.length)
      if(animalList.includes(animals[r])) {
        i--;
        continue;
      }
      animalList.push(animals[r])      
    }
    animalList = shuffle(animalList)
    if(this.props.attr.hasOwnProperty('text'))
      this.state = {
        animals: animalList.map(function(animal) { return {animal:animal, text: removeSentences(props.difficulty,splitIntoSentences(props.attr.text(animal)))}; }),
        revealed: false
      };
    else
      this.state = {
        animals: animalList.map(function(animal) { return {animal:animal, image: props.attr.image(animal)}; }),
        revealed: false
      };
  }

  revealNth(i,n) {
    let animalList = this.state.animals.slice()
    animalList[i].text = revealNth(n)(animalList[i].text)
    this.setState({animals: animalList })
  }

  revealAnswer() {
    this.setState({revealed:true})
    this.props.onGuess()
  }

  render() {
    return (
      <div>
        <h3>{this.props.attr.attribute}</h3>
        { this.props.attr.hasOwnProperty('text')
          ?
          this.state.animals.map((animal,i) =>
            <div>
              <CheckCircle revealed={this.state.revealed} condition={animal.animal === this.animal} onClick={() => this.revealAnswer()} />
              <p lang="de" className="trivia">{printMaskedSentences(animal.animal,(n) => this.revealNth(i,n),animal.text)}</p>
            </div>
          )
          :
          this.state.animals.map((animal) => <img src={animal.image} alt="Abbildung des Tiers"/>)
        }
      </div>
    );
  }
}

class CheckCircle extends Component {
  render() {
    if(! this.props.revealed)
      return <img className="checkcircle" src={emptyCheckCircle} onClick={() => this.props.onClick()} alt="empty check circle"/>;
    if(this.props.condition === true)
      return <img className="checkcircle" src={yesCheckCircle} onClick={() => this.props.onClick()} alt="yes check circle"/>;
    if(this.props.condition === false)
      return <img className="checkcircle" src={noCheckCircle} onClick={() => this.props.onClick()} alt="no check circle"/>;
  }
}

class GuessTrivia extends Component {
  constructor(props) {
    super(props);
    this.state = {
      lastAttr: 1
    };
  }
  
  nextTrivia() {
    this.setState({lastAttr:this.state.lastAttr+1})
  }
  
  render() {
    return (
      <div>
        <h1>Errate Trivia des Tieres {this.props.animal.species[0]}</h1>
        {animalAttributes.slice(0,this.state.lastAttr).map((attr,i) => <AnimalTrivia key={i.toString()} animal={this.props.animal} attr={animalAttributes[i]} difficulty={this.props.difficulty} onGuess={() => this.nextTrivia()} />)}
      </div>
    );
  }
}


class GameSelector extends Component {
  constructor(props) {
    super(props);
    this.difficulties = [
      { label: "Einfach", difficulty: 1 },
      { label: "Mittel", difficulty: 0.75 },
      { label: "Schwer", difficulty: 0.5 },
      { label: "Sehr Schwer", difficulty: 0.25 },
      { label: "Extrem", difficulty: 0.01 }
    ];
    this.state = {
      game:null,
      animal:null
    }
  }

  setDifficulty(difficulty) {
    this.props.onSelection(this.state.game,this.state.animal,difficulty)
  }

  setAnimal(animal) {
    this.setState({animal:animal})
  }
  
  selectRandomAnimal() {
    let i = Math.floor(Math.random() * (animals.length-1))
    this.setAnimal(animals[i])
  }
  
  guessAnimal() {
    this.setState({game:"guessAnimal"})
  }

  guessTrivia() {
    this.setState({game:"guessTrivia"})
  }

  render() {
    if(!this.state.game)
      return (
        <div>
          <h1>Wähle ein Spiel</h1>
          <button onClick={() => this.guessAnimal()}>Rate das Tier</button>
          <button onClick={() => this.guessTrivia()}>Tier Trivia</button>
        </div>
      );

    if(!this.state.animal)
      return (
        <div>
          <h1>Wähle ein Tier</h1>
          <p><button onClick={() => this.selectRandomAnimal()}>Zufällig</button></p>
          {animals.map((animal,i) => <button key={i.toString()} onClick={() => this.setAnimal(animal)}>{i}</button>)}
        </div>
      );

    return (
      <div>
        <h1>Wähle eine Schwierigkeitsstuffe</h1>
        {this.difficulties.map((d,i) =>
         <button key={i.toString()} onClick={() => this.setDifficulty(d.difficulty)}>{d.label}</button>)}
      </div>
    );
  }
}

class App extends Component {
  constructor() {
    super();
    this.state = {
      game: null,
      animal: null,
      difficulty: null
    }
  }
  
  selectAnimal(game,animal,difficulty) {
    this.setState({game:game, animal:animal, difficulty:difficulty})
  }

  render() {
    if(! this.state.animal)
      return <GameSelector onSelection={(game,animal,difficulty) => this.selectAnimal(game,animal,difficulty)}/>;
    else if(this.state.game === "guessAnimal")
      return <GuessAnimal animal={this.state.animal} difficulty={this.state.difficulty}/>;
    else
      return <GuessTrivia animal={this.state.animal} difficulty={this.state.difficulty}/>;
  }
}

export default App;
