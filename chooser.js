// chooser.js – picks the right persona block (1-6) and exposes window.currentBlock
const prefs = JSON.parse(localStorage.getItem('prefs') || '{}');

function hashCode(s){
  return s.split("").reduce((a,b)=>{a=((a<<5)-a)+b.charCodeAt(0);return a&a},0);
}

const key = `${prefs.maxFee||0}${prefs.maxWalk||0}${prefs.minCO2||0}${prefs.maxDay||0}${prefs.mode||3}`;
const blockID = (hashCode(key) % 6) + 1;          // 1-6
window.currentBlock = PERSONA_BLOCKS.filter(r => r.BlockID === blockID);

// analytics helper
if(typeof gtag !== 'undefined') gtag('event','persona',{persona_id:blockID});
console.log('Persona block loaded:',blockID);