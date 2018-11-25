const logBase = (base, x) => Math.log(x) / Math.log(base);
const ln = (x) => logBase(10,x)

export const uniformDist = (l,u) => Math.random() * (u - l) + l
export const expDist = (lam) => (l,u) => -lam * ln(uniformDist(Math.exp(-lam*u),Math.exp(-lam*l)))

export function shuffle(array) {
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
