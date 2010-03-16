// Copied from pattern cookbook.

/**
 * From PG_Cookbook01_Basic_Sequencing
 */

(
TempoClock.default.tempo = 84/60;

p = Pbind(
	\scale, #[0, 2, 3, 5, 7, 8, 10],
	\root, 2,
	\degree, Pseq(#[rest, 4, 3, 4, 2, 4, 1, 4, 0, 4, -0.9, 4, 0, 4, 1, 4, 2, 4,
		-3, 4, -1.9, 4, -0.9, 4, 0, 4, -0.9, 4, 0, 4, 1, 4, 2], 1),
	\dur, 0.25
).play;
)

(
TempoClock.default.tempo = 84/60;

p = Pbind(
	\instrument, \simple
	\scale, #[0, 2, 3, 5, 7, 8, 10],
	\root, 2,
	\degree, Pseq(#[rest, 4, 3, 4, 2, 4, 1, 4, 0, 4, -0.9, 4, 0, 4, 1, 4, 2, 4,
		-3, 4, -1.9, 4, -0.9, 4, 0, 4, -0.9, 4, 0, 4, 1, 4, 2], 1),
	\dur, 0.25
).play;
)


(
p = Pbind(
	\scale, #[0, 2, 3, 5, 7, 8, 10],
	\root, 2,
	\degree, Place([#[rest, 3, 2, 1, 0, -0.9, 0, 1, 2, -3, -1.9, -0.9, 0, -0.9, 0, 1, 2],
		(4 ! 16) ++ \rest], 17),
	\dur, 0.25
).play;
)

(
p = Pbind(
	\scale, #[0, 2, 3, 5, 7, 8, 10],
	\root, 2,
	\degree, Ppatlace([Pseq(#[rest, 3, 2, 1, 0, -0.9, 0, 1, 2, -3, -1.9, -0.9, 0, -0.9, 0, 1, 2], 1),
		Pn(4, 16)], inf),
	\dur, 0.25
).play;
)

(
p = Pbind(
	\degree, Pseries(7, Pwhite(1, 3, inf) * Prand(#[-1, 1], inf), inf).fold(0, 14)
	+ Prand(#[[0, -2, -4], [0, -3, -5], [0, -2, -5], [0, -1, -4]], inf),
	\dur, Pwrand(#[1, 0.5], #[0.8, 0.2], inf)
).play;
)

p.stop;

(
p = Pbind(
	\degree, 
	Pseries(7, Pwhite(1,3, inf) * Prand(#[-1,1], inf), inf).fold(0, 14) +
    Prand(#[[0, -2, -4], [0, -3, -5], [0, -2, -5], [0, -1, -4]], inf),
	\dur, Pwrand(#[1, 0.5], #[0.8, 0.2]));
)

p.stop;
p = Pwrand(#[1, 0.5], #[0.8, 0.2]);
100.do {p.asStream.next.postln;}


\time, Pkey(\delta) / Pfunc { thisThread.clock.tempo },

(
b = Buffer.read(s, "/home/atsuro/audio/wav/a11wlk01.wav");

SynthDef(\stretchedFragments, { |out, bufnum, start, time = 1, stretch = 1, amp = 1, attack = 0.01, decay = 0.05|
	var sig = PlayBuf.ar(1, bufnum, rate: stretch.reciprocal, startPos: start, doneAction:2);
	sig = PitchShift.ar(sig, pitchRatio: stretch)
	* EnvGen.kr(Env.linen(attack, time, decay), doneAction: 2);
	Out.ar(out, sig ! 2)
}).memStore; // note memStore! Without this, arguments won't work
)

(
TempoClock.default.tempo = 1;

p = Pbind(
	\instrument, \stretchedFragments,
	\bufnum, b,
	\start, Pwhite(0, (b.numFrames * 0.7).asInteger, inf),
	\delta, Pexprand(0.2, 1.5, inf),
	\time, Pkey(\delta),
	\stretch, Pexprand(1.0, 4.0, inf),
	\amp, 0.5,
	\attack, 0.1,
	\decay, 0.2
).play;
)

p.stop;
b.free; // be tidy! remember to clean up your Buffer

/**
 * From PG_Cookbook02_Manipulating_Patterns
 */

// Merging (interleaving) independent streams

(
var melodies = (
	lowMelody: Pseries(4, Prand(#[-2, -1, 1, 2], inf), inf).fold(-7, 11),
	highMelody: Pseries(14, Prand(#[-3, -2, 2, 3], inf), inf).fold(7, 18)
);

p = Pbind(
	\degree, Pnsym1(Pwrand(#[lowMelody, highMelody], [0.7, 0.3], inf), melodies),
	\dur, Pwrand(#[0.25, 0.5], #[0.4, 0.6], inf)
).play;
)

p.stop;


// Reading an array forward or backward arbitrarily

(
var pitches = (0..14), // replace with other pitches you want
move = 2,
window, slider;

// window = Window.new("Mouse Transport", Rect(5, 100, 500, 50));
// slider = Slider.new(window, Rect(5, 5, 490, 20))
// .action_({ |view|
// move = (view.value * 4 - 2).round;
// })
// .value_(0.5);
// window.front;

p = Pbind(
	// Pfunc is the direction to move through the array
	// it could be anything
	//   - could read from MIDI or HID and convert it into a step
	//   - could be a GUI control, as it is here
	\degree, Pwalk(pitches, Pfunc { move }, 1, 7),
	\dur, 0.25
).play;
)

p.stop;


// Changing Pbind value patterns on the fly

(
~degree = PatternProxy(Pn(Pseries(0, 1, 8), inf));
~dur = PatternProxy(Pn(0.25, inf));

p = Pbind(
	\degree, ~degree,
	\dur, ~dur
).play;
)

~degree.source = (Pexprand(1, 8, inf) - 1).round;

~dur.source = Pwrand(#[0.25, 0.5, 0.75], #[0.5, 0.3, 0.2], inf);

p.stop;


(
Pdefn(\degree, Pn(Pseries(0, 1, 8), inf));
Pdefn(\dur, Pn(0.25, inf));

p = Pbind(
	\degree, Pdefn(\degree),
	\dur, Pdefn(\dur)
).play;
)

Pdefn(\degree, (Pexprand(1, 8, inf) - 1).round);

Pdefn(\dur, Pwrand(#[0.25, 0.5, 0.75], #[0.5, 0.3, 0.2], inf));

p.stop;


/**
 * From PG_Cookbook03_External_Control
 */


(
var pattern = Pbind(
	\degree, Pseries(7, Pwhite(1, 3, inf) * Prand(#[-1, 1], inf), inf).fold(0, 14)
	+ Prand(#[[0, -2, -4], [0, -3, -5], [0, -2, -5], [0, -1, -4]], inf),
	\dur, Pwrand(#[1, 0.5], #[0.8, 0.2], inf)
),
player;

// Quicky GUI to tune threshold and decay times
~w = Window("threshold setting", Rect(15, 100, 300, 100))
.onClose_({
	~ampSynth.free;
	~ampUpdater.remove;
	~oscTrigResp.remove;
	player.stop;
});
~w.view.decorator = FlowLayout(~w.view.bounds, 2@2, 2@2);
~ampView = EZSlider(~w, 295@20, "amplitude", \amp, labelWidth: 80, numberWidth: 60);
~ampView.sliderView.canFocus_(false).enabled_(false);
~ampView.numberView.canFocus_(false).enabled_(false);
StaticText(~w, 295@5).background_(Color.gray);
~threshView = EZSlider(~w, 295@30, "threshold", \amp, action: { |ez|
	~ampSynth.set(\thresh, ez.value);
}, initVal: 0.4, labelWidth: 80, numberWidth: 60);
~decayView = EZSlider(~w, 295@30, "decay", #[0.1, 100, \exp], action: { |ez|
	~ampSynth.set(\decay, ez.value);
}, initVal: 80.0, labelWidth: 80, numberWidth: 60);

~w.front;

~ampSynth = SynthDef(\ampSynth, { |inbus, thresh = 0.8, decay = 1|
	var amp = Amplitude.kr(In.ar(inbus, 1), attackTime: 0.01, releaseTime: decay);
	// this trigger (id==0) is to update the gui only
	SendTrig.kr(Impulse.kr(10), 0, amp);
	// this trigger gets sent only when amplitude crosses threshold
	SendTrig.kr(amp >= thresh, 1, 1);
}).play(args: [inbus: s.options.numOutputBusChannels, thresh: ~threshView.value, decay: ~decayView.value]);

~ampUpdater = OSCpathResponder(s.addr, ['/tr', ~ampSynth.nodeID, 0], { |time, resp, msg|
	defer { ~ampView.value = msg[3] }
}).add;

~oscTrigResp = OSCpathResponder(s.addr, ['/tr', ~ampSynth.nodeID, 1], { |time, resp, msg|
	if(player.isNil or: { player.isPlaying.not }) {
		player = pattern.play;
	} {
		player.stop;
	};
}).add;
)

/**
 * From PG_Cookbook05_Using_Samples 
 */

(
b = Buffer.read(s, "/home/atsuro/audio/wav/a11wlk01.wav");

// one loop segment
SynthDef(\oneLoop, { |out, bufnum, start, time, amp|
	var sig = PlayBuf.ar(1, bufnum, startPos: start, loop: 0),
	env = EnvGen.kr(Env.linen(0.01, time, 0.05, level: amp), doneAction: 2);
	Out.ar(out, (sig * env) ! 2)
}).memStore;

SynthDef(\bell, { |out, accent = 0, amp = 0.1, decayScale = 1|
	var exc = PinkNoise.ar(amp) * Decay2.kr(Impulse.kr(0), 0.01, 0.05),
	sig = Klank.ar(`[
		{ ExpRand(400, 1600) } ! 4,
		1 ! 4,
		{ ExpRand(0.1, 0.4) } ! 4
	], exc, freqscale: accent + 1, decayscale: decayScale);
	DetectSilence.ar(sig, doneAction: 2);
	Out.ar(out, sig ! 2)
}).memStore;
)

(
TempoClock.default.tempo = 0.35953685899971 * 4;

p = Ptpar([
	0, Pbind(
		\instrument, \oneLoop,
		\bufnum, b,
		\amp, 0.4,
		\start, 17841,
		\time, 0.35953685899971.reciprocal,
		\dur, 4
	),
	0.5, Pn(
		Pfindur(4,
			Pbind(
				\instrument, \bell,
				\accent, Pseq([2, Pn(0, inf)], 1),
				\amp, Pseq([0.3, Pn(0.1, inf)], 1),
				\decayScale, Pseq([6, Pn(1, inf)], 1),
				\dur, Pwrand(#[0.25, 0.5, 0.75, 1], #[2, 3, 1, 1].normalizeSum, inf)
			)
		), 
		inf),
	0.5, Pbind(
		\instrument, \bell,
		\accent, -0.6,
		\amp, 0.2,
		\decayScale, 0.1,
		\dur, 1
	)
], 1).play;
)


s.quit;
p.stop;

// Example from DetectSilence Help file

(
SynthDef("detectSilence-help", { arg out;
	var z;
	z = SinOsc.ar(Rand(400, 700), 0, LFNoise2.kr(8, 0.2).max(0));
	DetectSilence.ar(z, doneAction:2);
	Out.ar(out, z);
}).send(s);
)
s.sendMsg("/s_new", "detectSilence-help", -1);
s.sendMsg("/s_new", "detectSilence-help", -1);
s.sendMsg("/s_new", "detectSilence-help", -1);

(
t = Task({
	loop({
		s.sendMsg("/s_new", "detectSilence-help", -1);
		[0.5, 1].choose.wait;
	})
}).play;
)

t.stop;

// Example from SendTrig help file
s.boot;
s.quit;
s.notify;

{ SendTrig.kr(Dust.kr(1.0),0,0.9) }.play;

// register to receive this message
(
OSCresponder(s.addr,'/tr',{ arg time,responder,msg;
	[time,responder,msg].postln;
}).add
);

// Example from "Using audio samples to play pitched material"

(
var recorder;
fork {
	b = Buffer.alloc(s, 44100 * 2, 1);
	s.sync;
	recorder = { |freq = 440|
		var initPulse = Impulse.kr(0),
		mod = SinOsc.ar(freq) * Decay2.kr(initPulse, 0.01, 3) * 5,
		car = SinOsc.ar(freq + (mod*freq)) * Decay2.kr(initPulse, 0.01, 2.0);
		RecordBuf.ar(car, b, loop: 0, doneAction: 2);
		car ! 2
	}.play;
	OSCpathResponder(s.addr, ['/n_end', recorder.nodeID], { |time, resp, msg|
		"done recording".postln;
		resp.remove;
	}).add;
};
SynthDef(\sampler, { |out, bufnum, freq = 1, amp = 1|
	var sig = PlayBuf.ar(1, bufnum, rate: freq, doneAction: 2) * amp;
	Out.ar(out, sig ! 2)
}).memStore;
)

(
// WAIT for "done recording" message before doing this
var samplerEvent = Event.default.put(\freq, { ~midinote.midicps / ~sampleBaseFreq });

TempoClock.default.tempo = 1;
p = Pbind(
	\degree, Pwhite(0, 12, inf),
	\dur, Pwrand([0.25, Pn(0.125, 2)], #[0.8, 0.2], inf),
	\amp, Pexprand(0.1, 0.5, inf),
	\sampleBaseFreq, 440,
	\instrument, \sampler,
	\bufnum, b
).play(protoEvent: samplerEvent);
)

p.stop;
b.free;


// Example from Multi-sampled instruments

(
f = { |val, array|
	var a, b, div;
	var i = array.indexOfGreaterThan(val);
	if(i.isNil) { array.size - 1 } {
		if(i == 0) { i } {
			a = array[i-1]; b = array[i];
			div = b / a;
			if(div == 1) { i } {
				// log() / log() == log(val/a) at base (b/a)
				// which is the inverse of exponential interpolation
				log(val / a) / log(div) + i - 1
			}
		}
	};
};
)



// Example of multi sampler

(
var bufCount;
~midinotes = (39, 46 .. 88);
bufCount = ~midinotes.size;

fork {
	// record the samples at different frequencies
	b = Buffer.allocConsecutive(~midinotes.size, s, 44100 * 2, 1);
	SynthDef(\sampleSource, { |freq = 440, bufnum|
		var initPulse = Impulse.kr(0),
		mod = SinOsc.ar(freq) * Decay2.kr(initPulse, 0.01, 3) * 5,
		car = SinOsc.ar(freq + (mod*freq)) * Decay2.kr(initPulse, 0.01, 2.0);
		RecordBuf.ar(car, bufnum, loop: 0, doneAction: 2);
	}).send(s);
	s.sync;
	// record all 8 buffers concurrently
	b.do({ |buf, i|
		Synth(\sampleSource, [freq: ~midinotes[i].midicps, bufnum: buf]);
	});
};
OSCresponderNode(s.addr, '/n_end', { |t, r, m|
	bufCount = bufCount - 1;
	if(bufCount == 0) {
		"done recording".postln;
		r.remove;
	};
}).add;

SynthDef(\multiSampler, { |out, bufnum, bufBase, baseFreqBuf, freq = 440, amp = 1|
	var buf1 = bufnum.floor,
	buf2 = buf1 + 1,
	xfade = (bufnum - buf1).madd(2, -1),
	basefreqs = Index.kr(baseFreqBuf, [buf1, buf2]),
	playbufs = PlayBuf.ar(1, bufBase + [buf1, buf2], freq / basefreqs, loop: 0, doneAction: 2),
	sig = XFade2.ar(playbufs[0], playbufs[1], xfade, amp);
	Out.ar(out, sig ! 2)
}).memStore;

~baseBuf = Buffer.alloc(s, ~midinotes.size, 1, { |buf| buf.setnMsg(0, ~midinotes.midicps) });
)

(
TempoClock.default.tempo = 1;
p = Pbind(
	\instrument, \multiSampler,
	\bufBase, b.first,
	\baseFreqBuf, ~baseBuf,
	\degree, Pseries(0, Prand(#[-2, -1, 1, 2], inf), inf).fold(-11, 11),
	\dur, Pwrand([0.25, Pn(0.125, 2)], #[0.8, 0.2], inf),
	\amp, Pexprand(0.1, 0.5, inf),
	// some important conversions
	// identify the buffer numbers to read
	\freq, Pfunc { |ev| ev.use(ev[\freq]) },
	\bufnum, Pfunc({ |ev| ~midinotes.indexInBetween(ev[\freq].cpsmidi) })
	.clip(0, ~midinotes.size - 1.001)
).play;
)

p.stop;
b.do(_.free); ~baseBuf.free;

[100,101,102,103,104,105].indexInBetween(105.5);

/**
 * From PG_Cookbook06_Phrase_Network
 */

a = #[1, 1, 1, 1, 2, 2, 3, 3, 4, 4, 5];
({ a.choose } ! 100).histo(5, 1, 5);

a = WeighBag.with((1..5), #[4, 2, 2, 2, 1]);
({ a.wchoose } ! 100).histo(5, 1, 5);

// Tests for envelope shape
Env.adsr(0, 0.3, 0.1, 20).test;
Env.adsr(0.04, 0.2, 0.6, 0.1).test;

(
// this SynthDef has a strong attack, emphasizing the articulation
SynthDef(\sawpulse, { |out, freq = 440, gate = 0.5, plfofreq = 6, mw = 0, ffreq = 2000, rq = 0.3, freqlag = 0.05, amp = 1|
	var sig, plfo, fcurve;
	plfo = SinOsc.kr(plfofreq, mul:mw, add:1);
	freq = Lag.kr(freq, freqlag) * plfo;
	fcurve = EnvGen.kr(Env.adsr(0, 0.3, 0.1, 20), gate);
	fcurve = (fcurve - 1).madd(0.7, 1) * ffreq;
	sig = Mix.ar([Pulse.ar(freq, 0.9), Saw.ar(freq*1.007)]);
	sig = RLPF.ar(sig, fcurve, rq)
	* EnvGen.kr(Env.adsr(0.04, 0.2, 0.6, 0.1), gate, doneAction:2)
	* amp;
	Out.ar(out, sig ! 2)
}).memStore;
)

(
TempoClock.default.tempo = 128/60;

// Pmul does only one thing here: take ~amp from each event
// and replace it with ~amp * 0.4
p = Pmul(\amp, 0.4, Pfsm([
	#[0, 3, 1], // starting places
	// phrase 1
	PmonoArtic(\sawpulse,
		\midinote, Pseq([78, 81, 78, 76, 78, 76, 72, 71, 69, 66], 1),
		\dur, Pseq(#[0.25, 1.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25], 1),
		\sustain, Pseq(#[0.3, 1.2, 0.3, 0.2, 0.3, 0.2, 0.3, 0.2, 0.3, 0.2],1 ),
		\amp, Pseq(#[1, 0.5, 0.75, 0.5, 0.75, 0.5, 0.75, 0.5, 0.75, 0.5], 1),
		\mw, Pseq([0, 0.03, Pseq(#[0], inf)], 1)
	), #[1, 2, 3, 4, 7],
	// phrase 2
	PmonoArtic(\sawpulse,
		\midinote, Pseq([64, 66, 69, 71, 72, 73], 1),
		\dur, Pseq(#[0.25], 6),
		\sustain, Pseq(#[0.3, 0.2, 0.2, 0.2, 0.3, 0.2], 1),
		\amp, Pseq(#[1, 0.5, 0.5, 0.5, 0.5, 0.5], 1),
		\mw, 0
	), #[1, 1, 1, 1, 2, 2, 3, 3, 4, 4, 5],
	// phrase 3
	PmonoArtic(\sawpulse,
		\midinote, Pseq([69, 71, 69, 66, 64, 69, 71, 69], 1),
		\dur, Pseq(#[0.125, 0.625, 0.25, 0.25, 0.25, 0.25, 0.25, 0.75], 1),
		\sustain, Pseq(#[0.2, 0.64, 0.2, 0.2, 0.2, 0.3, 0.3, 0.75], 1),
		\amp, Pseq(#[0.5, 0.75, 0.5, 0.5, 0.5, 1, 0.5, 0.5], 1),
		\mw, 0
	), #[0, 1, 1, 1, 1, 3, 3, 3, 3, 5],
	// phrase 4
	PmonoArtic(\sawpulse,
		\midinote, Pseq([72, 73, 76, 72, 71, 69, 66, 71, 69], 1),
		\dur, Pseq(#[0.25, 0.25, 0.25, 0.083, 0.083, 0.084, 0.25, 0.25, 0.25], 1),
		\sustain, Pseq(#[0.3, 0.2, 0.2, 0.1, 0.07, 0.07, 0.2, 0.3, 0.2], 1),
		\amp, Pseq(#[1, 0.5, 0.5, 1, 0.3, 0.3, 0.75, 0.75, 0.5], 1),
		\mw, 0
	), #[1, 1, 1, 1, 3, 3, 4, 4, 4],
	// phrase 5
	PmonoArtic(\sawpulse,
		\midinote, Pseq([64, 66, 69, 71, 72, 73, 71, 69, 66, 71, 69, 66, 64, 69], 1),
		\dur, Pseq(#[0.25, 0.25, 0.25, 0.25, 0.125, 0.375, 0.166, 0.166, 0.168,
			0.5, 0.166, 0.166, 0.168, 0.5], 1),
		\sustain, Pseq(#[0.3, 0.2, 0.2, 0.2, 0.14, 0.4, 0.2, 0.2, 0.2, 0.6, 0.2, 0.2, 0.2, 0.5],1),
		\amp, Pseq(#[0.5, 0.5, 0.6, 0.8, 1, 0.5, 0.5, 0.5, 0.5, 1,
			0.5, 0.5, 0.5, 0.45], 1),
		\mw, 0
	), #[0, 1, 1, 1, 1, 3, 3, 5],

	// phrase 6 
	PmonoArtic(\sawpulse,
		\midinote, Pseq([72, 73, 76, 78, 81, 78, 83, 81, 84, 85], 1),
		\dur, Pseq(#[0.25, 0.25, 0.25, 0.25, 0.5, 0.5, 0.5, 0.5, 0.125, 1.125], 1),
		\sustain, Pseq(#[0.3, 0.2, 0.2, 0.2, 0.95, 0.25, 0.95, 0.25, 0.2, 1.13], 1),
		\amp, Pseq(#[0.7, 0.5, 0.5, 0.5, 0.7, 0.5, 0.8, 0.5, 1, 0.5], 1),
		\mw, Pseq([Pseq(#[0], 9), 0.03], 1)
	), #[6, 6, 6, 8, 9, 10, 10, 10, 10, 11, 11, 13, 13],
	// phrase 7 
	PmonoArtic(\sawpulse,
		\midinote, Pseq([83, 81, 78, 83, 81, 78, 76, 72, 73, 78, 72, 72, 71], 1),
		\dur, Pseq(#[0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 
			0.25, 2], 1),
		\sustain, Pseq(#[0.3, 0.3, 0.2, 0.3, 0.3, 0.3, 0.2, 0.3, 0.2, 0.3, 0.2, 0.3, 2], 1),
		\amp, Pseq(#[0.5, 0.5, 0.5, 0.8, 0.5, 0.5, 0.5, 0.8, 0.5, 0.8, 0.5,
			1, 0.4], 1),
		\mw, Pseq([Pseq([0], 12), 0.03], 1)
	), #[0, 7, 7, 7, 7, 7, 3, 3, 3, 3],
	// phrase 8 
	PmonoArtic(\sawpulse,
		\midinote, Pseq([69, 71, 72, 71, 69, 66, 64, 69, 71], 1),
		\dur, Pseq(#[0.25, 0.25, 0.25, 0.25, 0.166, 0.167, 0.167, 0.25, 0.25], 1),
		\sustain, Pseq(#[0.2, 0.2, 0.3, 0.2, 0.2, 0.2, 0.14, 0.3, 0.2], 1),
		\amp, Pseq(#[0.5, 0.5, 0.8, 0.5, 0.5, 0.5, 0.5, 0.8, 0.5], 1)
	), #[3, 3, 3, 4, 4, 5],
	// phrase 9 
	PmonoArtic(\sawpulse,
		\midinote, Pseq([84, 85, 84, 84, 88, 84, 83, 81, 83, 81, 78, 76, 81, 83], 1),
		\dur, Pseq(#[0.125, 0.535, 0.67, 1.92, 0.25, 0.166, 0.167, 0.167, 
			0.25, 0.25, 0.25, 0.25, 0.25, 0.25], 1),
		\sustain, Pseq(#[0.2, 3.12, 0.2, 0.2, 0.2, 0.2, 0.2, 0.15, 0.3, 0.2, 0.2, 0.2,
			0.3, 0.2], 1),
		\amp, Pseq(#[1, 0.8, 0.8, 0.8, 1, 1, 0.8, 0.8, 1, 0.8, 0.8, 0.8,
			1, 0.8], 1),
		\mw, Pseq([0, 0.005, 0.005, 0.06, Pseq(#[0], 10)], 1)
	), #[10, 10, 10, 11, 11, 11, 11, 12, 12, 12],
	// phrase 10, same as #4, 8va
	PmonoArtic(\sawpulse,
		\midinote, Pseq(([64, 66, 69, 71, 72, 73, 71, 69, 66, 71, 69, 66, 64, 69]+12), 1),
		\dur, Pseq(#[0.25, 0.25, 0.25, 0.25, 0.125, 0.375, 0.166, 0.166, 0.168,
			0.5, 0.166, 0.166, 0.168, 0.5], 1),
		\sustain, Pseq(#[0.3, 0.2, 0.2, 0.2, 0.14, 0.4, 0.2, 0.2, 0.2, 0.6, 0.2, 0.2, 0.2, 0.5],1),
		\amp, Pseq(#[0.5, 0.5, 0.6, 0.8, 1, 0.5, 0.5, 0.5, 0.5, 1,
			0.5, 0.5, 0.5, 0.45], 1),
		\mw, 0
	), #[11, 11, 11, 11, 11, 12, 12],
	// phrase 11
	PmonoArtic(\sawpulse,
		\midinote, Pseq([81, 84, 83, 81, 78, 76, 81, 83], 1),
		\dur, Pseq(#[0.25], 8),
		\sustain, Pseq(#[0.2, 0.3, 0.3, 0.2, 0.3, 0.2, 0.3, 0.2], 1),
		\amp, Pseq(#[0.5, 1, 0.5, 0.5, 0.6, 0.5, 0.8, 0.5], 1),
		\mw, 0
	), #[0, 9, 9, 11, 11, 12, 12, 12, 12, 12],
	// phrase 12, same as #1, 8va
	PmonoArtic(\sawpulse,
		\midinote, Pseq(([64, 66, 69, 71, 72, 73]+12), 1),
		\dur, Pseq(#[0.25], 6),
		\sustain, Pseq(#[0.3, 0.2, 0.2, 0.2, 0.3, 0.2], 1),
		\amp, Pseq(#[1, 0.5, 0.5, 0.5, 0.5, 0.5], 1),
		\mw, 0
	), #[6, 6, 8, 9, 9, 9, 9, 10, 10, 10, 10, 13, 13, 13],
	// phrase 13
	PmonoArtic(\sawpulse,
		\midinote, Pseq([78, 81, 83, 78, 83, 84, 78, 84, 85], 1),
		\dur, Pseq(#[0.25, 0.25, 0.5, 0.25, 0.25, 0.5, 0.25, 0.25, 1.75], 1),
		\sustain, Pseq(#[0.2, 0.3, 0.2, 0.2, 0.3, 0.2, 0.2, 0.3, 1.75], 1),
		\amp, Pseq(#[0.4, 0.8, 0.5, 0.4, 0.8, 0.5, 0.4, 1, 0.8], 1),
		\mw, Pseq([Pseq([0], 8), 0.03], 1)
	), #[8, 13, 13],
	// phrase 14
	PmonoArtic(\sawpulse, 
		\midinote, Pseq([88, 84, 83, 81, 83, 81, 78, 76, 81, 83], 1),
		\dur, Pseq(#[0.25, 0.166, 0.167, 0.167, 
			0.25, 0.25, 0.25, 0.25, 0.25, 0.25], 1),
		\sustain, Pseq(#[0.2, 0.2, 0.2, 0.15, 0.3, 0.2, 0.2, 0.2,
			0.3, 0.2], 1),
		\amp, Pseq(#[1, 1, 0.8, 0.8, 1, 0.8, 0.8, 0.8,
			1, 0.8], 1),
		\mw, 0
	), #[10]
], inf)).play;
)

s.dump;

p.stop;

/**
 * From PG_Cookbook07_Rhythmic_Variations
 */

(
~kik = Penvir(~kikEnvir, Pn(Plazy({
	~init.value;
	~addNotes.value;
	Pbindf(
		Pbind(
			\instrument, \kik,
			\preamp, 0.4,
			\dur, 0.25,
			*(~pbindPairs.value(#[amp, decay2]))
		),
		\freq, Pif(Pkey(\amp) > 0, 1, \rest)
	)
}), inf)).play(quant: 4);
)

~kik.stop;


(
// this kick drum doesn't sound so good on cheap speakers
// but if your monitors have decent bass, it's electro-licious
SynthDef(\kik, { |basefreq = 50, ratio = 7, sweeptime = 0.05, preamp = 1, amp = 1,
	decay1 = 0.3, decay1L = 0.8, decay2 = 0.15, out|
	var fcurve = EnvGen.kr(Env([basefreq * ratio, basefreq], [sweeptime], \exp)),
	env = EnvGen.kr(Env([1, decay1L, 0], [decay1, decay2], -4), doneAction: 2),
	sig = SinOsc.ar(fcurve, 0.5pi, preamp).distort * env * amp;
	Out.ar(out, sig ! 2)
}).memStore;

SynthDef(\kraftySnr, { |amp = 1, freq = 2000, rq = 3, decay = 0.3, pan, out|
	var sig = PinkNoise.ar(amp),
	env = EnvGen.kr(Env.perc(0.01, decay), doneAction: 2);
	sig = BPF.ar(sig, freq, rq, env);
	Out.ar(out, Pan2.ar(sig, pan))
}).memStore;

~commonFuncs = (
	// save starting time, to recognize the last bar of a 4-bar cycle
	init: {
		if(~startTime.isNil) { ~startTime = thisThread.clock.beats };
	},
	// convert the rhythm arrays into patterns
	pbindPairs: { |keys|
		var pairs = Array(keys.size * 2);
		keys.do({ |key|
			if(key.envirGet.notNil) { pairs.add(key).add(Pseq(key.envirGet, 1)) };
		});
		pairs
	},
	// identify rests in the rhythm array
	// (to know where to stick notes in)
	getRestIndices: { |array|
		var result = Array(array.size);
		array.do({ |item, i|
			if(item == 0) { result.add(i) }
		});
		result
	}
);
)

(
TempoClock.default.tempo = 104 / 60;

~kikEnvir = (
	parent: ~commonFuncs,
	// rhythm pattern that is constant in each bar
	baseAmp: #[1, 0, 0, 0,  0, 0, 0.7, 0,  0, 1, 0, 0,  0, 0, 0, 0] * 0.5,
	baseDecay: #[0.15, 0, 0, 0,  0, 0, 0.15, 0,  0, 0.15, 0, 0,  0, 0, 0, 0],
	addNotes: {
		var beat16pos = (thisThread.clock.beats - ~startTime) % 16,
		available = ~getRestIndices.(~baseAmp);
		~amp = ~baseAmp.copy;
		~decay2 = ~baseDecay.copy;
		// if last bar of 4beat cycle, do busier fills
		if(beat16pos.inclusivelyBetween(12, 16)) {
			available.scramble[..rrand(5, 10)].do({ |index|
				// crescendo
				~amp[index] = index.linexp(0, 15, 0.2, 0.5);
				~decay2[index] = 0.15;
			});
		} {
			available.scramble[..rrand(0, 2)].do({ |index|
				~amp[index] = rrand(0.15, 0.3);
				~decay2[index] = rrand(0.05, 0.1);
			});
		}
	}
);

~snrEnvir = (
	parent: ~commonFuncs,
	baseAmp: #[0, 0, 0, 0,  1, 0, 0, 0,  0, 0, 0, 0,  1, 0, 0, 0] * 1.5,
	baseDecay: #[0, 0, 0, 0,  0.7, 0, 0, 0,  0, 0, 0, 0,  0.4, 0, 0, 0],
	addNotes: {
		var beat16pos = (thisThread.clock.beats - ~startTime) % 16,
		available = ~getRestIndices.(~baseAmp),
		choice;
		~amp = ~baseAmp.copy;
		~decay = ~baseDecay.copy;
		if(beat16pos.inclusivelyBetween(12, 16)) {
			available.scramble[..rrand(5, 9)].do({ |index|
				~amp[index] = index.linexp(0, 15, 0.5, 1.8);
				~decay[index] = rrand(0.2, 0.4);
			});
		} {
			available.scramble[..rrand(1, 3)].do({ |index|
				~amp[index] = rrand(0.15, 0.3);
				~decay[index] = rrand(0.2, 0.4);
			});
		}
	}
);

~hhEnvir = (
	parent: ~commonFuncs,
	baseAmp: 15 ! 16,
	baseDelta: 0.25 ! 16,
	addNotes: {
		var beat16pos = (thisThread.clock.beats - ~startTime) % 16,
		available = (0..15),
		toAdd;
		// if last bar of 4beat cycle, do busier fills
		~amp = ~baseAmp.copy;
		~dur = ~baseDelta.copy;
		if(beat16pos.inclusivelyBetween(12, 16)) {
			toAdd = available.scramble[..rrand(2, 5)]
		} {
			toAdd = available.scramble[..rrand(0, 1)]
		};
		toAdd.do({ |index|
			~amp[index] = ~doubleTimeAmps;
			~dur[index] = ~doubleTimeDurs;
		});
	},
	doubleTimeAmps: Pseq(#[15, 10], 1),
	doubleTimeDurs: Pn(0.125, 2)
);

~kik = Penvir(~kikEnvir, Pn(Plazy({
	~init.value;
	~addNotes.value;
	Pbindf(
		Pbind(
			\instrument, \kik,
			\preamp, 0.4,
			\dur, 0.25,
			*(~pbindPairs.value(#[amp, decay2]))
		),
		// default Event checks \freq --
		// if a symbol like \rest or even just \,
		// the event is a rest and no synth will be played
		\freq, Pif(Pkey(\amp) > 0, 1, \rest)
	)
}), inf)).play(quant: 4);

~snr = Penvir(~snrEnvir, Pn(Plazy({
	~init.value;
	~addNotes.value;
	Pbindf(
		Pbind(
			\instrument, \kraftySnr,
			\dur, 0.25,
			*(~pbindPairs.value(#[amp, decay]))
		),
		\freq, Pif(Pkey(\amp) > 0, 5000, \rest)
	)
}), inf)).play(quant: 4);

~hh = Penvir(~hhEnvir, Pn(Plazy({
	~init.value;
	~addNotes.value;
	Pbindf(
		Pbind(
			\instrument, \kraftySnr,
			\rq, 0.06,
			\amp, 15,
			\decay, 0.04,
			*(~pbindPairs.value(#[amp, dur]))
		),
		\freq, Pif(Pkey(\amp) > 0, 12000, \rest)
	)
}), inf)).play(quant: 4);
)

// stop just before barline
t = TempoClock.default;
t.schedAbs(t.nextTimeOnGrid(4, -0.001), {
	[~kik, ~snr, ~hh].do(_.stop);
});

~hh.play;
~hh.stop;
(1..15).scramble[1..5]

~snr.play;
~snr.stop;

// Event
10.do { |i| i.linexp(0, 15, 0.5, 1.8).postln; }