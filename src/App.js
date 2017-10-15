import React, { Component } from 'react';
import './App.css';
import animals from './animals.json';

const shownSize = (sentence) => {
  if(sentence.masked === "shown")
    return sentence.text.length
  else
    return 0
};

const size = (sentences) => sentences.reduce((acc,sentence) => acc + shownSize(sentence),0)

const shown = (sentence) => {return {masked:"shown", text:sentence}};

const indeces = (s,i) => {return {id:i, masked:s.masked}};

const numShown = (maskedSentences) => maskedSentences.filter(s => s.masked === "shown").length

const removeSentences = (percent) => (sentences) => {
  var reduced = sentences.map(shown)
  let totalSize = size(reduced)
  while(size(reduced) > totalSize * percent && numShown(reduced) > 1) {
    let red = reduced.map(indeces).filter(s => s.masked === "shown")
    let r = Math.floor(Math.random() * (red.length-1))
    reduced[red[r].id].masked = "hidden"
  }
  return reduced
};

const reveal = (maskedSentences) => {
  let hidden = maskedSentences.map(indeces).filter(s => s.masked === "hidden")
  if(hidden.length > 0) {
    let r = Math.floor(Math.random() * (hidden.length-1))
    maskedSentences[hidden[r].id].masked = "shown"
  }
  return maskedSentences
}

const completelyRevealed = (maskedSentences) => maskedSentences.every(s => s.masked === "shown")

const printMaskedSentence = (maskedSentence) => {
  if(maskedSentence.masked === "shown")
    return maskedSentence.text
  else
    return "[...]"
};

const printMaskedSentences = (animal) => (maskedSentences) =>
  maskedSentences.map(compose(maskSpecies(animal),printMaskedSentence)).join(". ")

const splitIntoSentences = (text) => text.split(".").map(s => s.trim()).filter(s => s.length !== 0)

const maskSpecies = (animal) => (text) => {
  let species = animal.species.concat(animal.species.map(s => s.toLowerCase()))
  return species.reduce((t,s) => t.replace(s,"..."),text)
};
  
const animalAttributes = (animal) => {
  return [
    {attribute: "Verbreitung",    text: animal.distribution },
    {attribute: "Habitat",        text: animal.habitat },
    {attribute: "Ernährung",      text: animal.diet },
    {attribute: "Sozialstruktur", text: animal.socialStructure },
    {attribute: "Fortpflanzung",  text: animal.reproduction},
    {attribute: "Verhalten",      text: animal.behavior},
    {attribute: "Merkmale",       text: animal.characteristic},
    {attribute: "Bild",           image: animal.image}
  ]
}

const mapAttributeText = (f) => (attr) => attr.hasOwnProperty('text') ? Object.assign(attr,{text:f(attr.text)}) : attr

const compose = (...fns) => x => fns.reduceRight((v, f) => f(v), x);

class AnimalAttribute extends Component {
  constructor(props) {
    super(props)
    this.state = {
      attr: this.props.attr
    };
  }
  
  reveal() {
    this.setState({attr: mapAttributeText(reveal)(this.state.attr)})
  }

  render() {
    return (
      <div>
        <h3>{this.state.attr.attribute}</h3>
        { this.state.attr.hasOwnProperty('text')
          ?
          <div>
            <p>{printMaskedSentences(this.props.animal)(this.state.attr.text)}</p>
            <button disabled={completelyRevealed(this.state.attr.text)}
                    onClick={() => this.reveal()}>
              Tip
            </button>
          </div>
          :
          <img src={this.state.attr.image} alt="Abbildung des Tiers"/>
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

  answer(event) {
    var name = document.getElementById('guess').value
    document.getElementById('guess').value = ""
    this.setState({answer: name})
  }

  answered() {
    return this.state.answer !== null && this.props.animal.species.includes(this.state.answer)
  }

  resolve() {
    this.setState({resolved:true})
  }

  render() {
    if(this.state.resolved) {
      return (<p>Die Lösung ist {this.props.animal.species[0]}</p>);
    } else if(this.answered()) {
      return (
        <div>
          <p>Korrekt. Gratulation</p>
        </div>
      );
    } else {
      return (
        <div>
          {! this.props.furtherHints && <button onClick={() => this.props.onHint()}>Nächste Kategorie</button>}
          <p>
            Name des Tiers: <input id="guess" type="text"></input>
          <button onClick={() => this.answer()}>Tip abgeben</button>
          </p>
          {this.state.answer !== null &&
           !this.props.animal.species.includes(this.state.answer) &&
           <p>Nicht korrekt, rate weiter</p>}
          <button onClick={() => this.resolve()}>Auflösen</button>
        </div>
      );
    }
  }
}

class Game extends Component {
  constructor(props) {
    super(props);
    this.animal = this.props.animal
    this.attr = animalAttributes(this.animal).map(mapAttributeText(compose(removeSentences(this.props.difficulty),splitIntoSentences)))
    this.state = {
      lastAttr: 1,
      answer: null
    };
  }
  
  nextTip() {
    this.setState({lastAttr:this.state.lastAttr+1})
  }
  
  render() {
    return (
      <div>
        <h1>Errate das Tier</h1>
        {this.attr.slice(0,this.state.lastAttr).map((attr,i) => <AnimalAttribute key={i.toString()} animal={this.animal} attr={this.attr[i]}/>)}
        <Guesser animal={this.animal} onHint={() => this.nextTip()} furtherHints={this.state.lastAttr >= this.attr.length}/>
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
      animal:null
    }
  }

  setDifficulty(difficulty) {
    this.props.onSelection(this.state.animal,difficulty)
  }

  setAnimal(animal) {
    this.setState({animal:animal})
  }
  
  selectRandomAnimal() {
    let i = Math.floor(Math.random() * (animals.length-1))
    this.setAnimal(animals[i])
  }

  render() {
    if(!this.state.animal)
      return (
        <div>
          <h1>Wähle ein Tier</h1>
          <p><button onClick={() => this.selectRandomAnimal()}>Zufällig</button></p>
          {animals.map((animal,i) => <button key={i.toString()} onClick={() => this.setAnimal(animal)}>{i}</button>)}
        </div>
      );

    if(!this.state.difficulty)
      return (
        <div>
          <h1>Wähle eine Schwierigkeitsstuffe</h1>
          {this.difficulties.map((d,i) => <button key={i.toString()} onClick={() => this.setDifficulty(d.difficulty)}>{d.label}</button>)}
        </div>
      );
  }
}

class App extends Component {
  constructor() {
    super();
    this.state = {
      animal: null,
      difficulty: null
    }
  }
  
  selectAnimal(animal,difficulty) {
    this.setState({animal:animal, difficulty:difficulty})
  }

  render() {
    if(! this.state.animal)
      return <GameSelector onSelection={(animal,difficulty) => this.selectAnimal(animal,difficulty)}/>;
    else
      return <Game animal={this.state.animal} difficulty={this.state.difficulty}/>;
  }
}

export default App;
