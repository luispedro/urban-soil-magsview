// This is called BEFORE your Elm app starts up
// 
// The value returned here will be passed as flags 
// into your `Shared.init` function.
export const flags = () => {

}

// This is called AFTER your Elm app starts up
//
// Here you can work with `app.ports` to send messages
// to your Elm application, or subscribe to incoming
// messages from Elm
// FASTA cache: magId -> Map<contigName, sequence>
const fastaCache = new Map();
// In-flight fetches to avoid duplicate requests
const fastaFetching = new Map();

function parseFasta(text) {
  const contigs = new Map();
  let currentName = null;
  const chunks = [];
  for (const line of text.split('\n')) {
    if (line.startsWith('>')) {
      if (currentName !== null) {
        contigs.set(currentName, chunks.join(''));
        chunks.length = 0;
      }
      currentName = line.slice(1).split(/\s/)[0];
    } else {
      chunks.push(line.trim());
    }
  }
  if (currentName !== null) {
    contigs.set(currentName, chunks.join(''));
  }
  return contigs;
}

async function fetchFasta(magId) {
  if (fastaCache.has(magId)) {
    return fastaCache.get(magId);
  }
  if (fastaFetching.has(magId)) {
    return fastaFetching.get(magId);
  }
  const url = `https://sh-dog-mags-data.big-data-biology.org/ShanghaiDogsMAGs/${magId}.fna.gz`;
  const promise = (async () => {
    const response = await fetch(url);
    if (!response.ok) {
      throw new Error(`Failed to fetch FASTA: ${response.status}`);
    }
    const ds = new DecompressionStream('gzip');
    const decompressed = response.body.pipeThrough(ds);
    const reader = decompressed.getReader();
    const decoder = new TextDecoder();
    let text = '';
    while (true) {
      const { done, value } = await reader.read();
      if (done) break;
      text += decoder.decode(value, { stream: true });
    }
    text += decoder.decode();
    const contigs = parseFasta(text);
    fastaCache.set(magId, contigs);
    fastaFetching.delete(magId);
    return contigs;
  })();
  fastaFetching.set(magId, promise);
  return promise;
}

const complementMap = { A: 'T', T: 'A', G: 'C', C: 'G', a: 't', t: 'a', g: 'c', c: 'g', N: 'N', n: 'n' };

function reverseComplement(seq) {
  return seq.split('').reverse().map(c => complementMap[c] || 'N').join('');
}

const codonTable = {
  'TTT':'F','TTC':'F','TTA':'L','TTG':'L',
  'CTT':'L','CTC':'L','CTA':'L','CTG':'L',
  'ATT':'I','ATC':'I','ATA':'I','ATG':'M',
  'GTT':'V','GTC':'V','GTA':'V','GTG':'V',
  'TCT':'S','TCC':'S','TCA':'S','TCG':'S',
  'CAT':'H','CAC':'H','CAA':'Q','CAG':'Q',
  'CCT':'P','CCC':'P','CCA':'P','CCG':'P',
  'ACT':'T','ACC':'T','ACA':'T','ACG':'T',
  'GCT':'A','GCC':'A','GCA':'A','GCG':'A',
  'TAT':'Y','TAC':'Y','TAA':'*','TAG':'*',
  'TGT':'C','TGC':'C','TGA':'*','TGG':'W',
  'CGT':'R','CGC':'R','CGA':'R','CGG':'R',
  'AGT':'S','AGC':'S','AGA':'R','AGG':'R',
  'AAT':'N','AAC':'N','AAA':'K','AAG':'K',
  'GAT':'D','GAC':'D','GAA':'E','GAG':'E',
  'GGT':'G','GGC':'G','GGA':'G','GGG':'G'
};

const startCodonsTable11 = new Set(['TTG', 'CTG', 'ATT', 'ATC', 'ATA', 'ATG', 'GTG']);

function translateDNA(dna) {
  const protein = [];
  const upper = dna.toUpperCase();
  for (let i = 0; i + 2 < upper.length; i += 3) {
    const codon = upper.slice(i, i + 3);
    if (i === 0 && startCodonsTable11.has(codon)) {
      protein.push('M');
    } else {
      protein.push(codonTable[codon] || 'X');
    }
  }
  return protein.join('');
}

export const onReady = ({ app, env }) => {
  let sc = document.createElement('script');
  sc.setAttribute('src', "https://www.googletagmanager.com/gtag/js?id=G-60E4QKLHYR");
  sc.setAttribute('async', true);
  document.getElementById('google-injection-site').appendChild(sc);

  window.dataLayer = window.dataLayer || [];
  function gtag(){dataLayer.push(arguments);}
  gtag('js', new Date());

  gtag('config', 'G-60E4QKLHYR');
  gtag('send', 'pageview');
  app.ports.updatePath.subscribe(function(path) {
      gtag('set', 'page', '/'+path);
      gtag('send', 'pageview');
  });

  app.ports.copyToClipboard.subscribe(function(args) {
    const { text, buttonId } = args;
    navigator.clipboard.writeText(text).then(function() {
      const btn = document.getElementById(buttonId);
      if (btn) {
        btn.textContent = '\u2713';
        btn.classList.add('copy-btn-copied');
        setTimeout(function() {
          btn.textContent = '\u{1F4CB}';
          btn.classList.remove('copy-btn-copied');
        }, 1500);
      }
    });
  });

  app.ports.requestGeneSequence.subscribe(async function(request) {
    try {
      const { magId, contig, start, end, strand, seqid } = request;
      const contigs = await fetchFasta(magId);
      const contigSeq = contigs.get(contig);
      if (!contigSeq) {
        app.ports.receiveGeneSequence.send({ error: `Contig ${contig} not found in FASTA` });
        return;
      }
      // Coordinates are 1-based from eggnog-mapper
      let dna = contigSeq.slice(start - 1, end);
      if (strand === '-') {
        dna = reverseComplement(dna);
      }
      const protein = translateDNA(dna);
      app.ports.receiveGeneSequence.send({ dna, protein });
    } catch (err) {
      app.ports.receiveGeneSequence.send({ error: err.message || String(err) });
    }
  });
}
