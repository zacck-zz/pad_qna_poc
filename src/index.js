import { Elm } from './Main.elm'

let mediaRecorder;
let recordedAudio =  [];

const app = Elm.Main.init({
  node: document.querySelector('main')
})

console.log(app)
app.ports.startRecording.subscribe(function() {
	console.log("Now we record audio");
	recordAudio();
})

app.ports.stopRecording.subscribe(function() {
	console.log("Now we are done recording audio");
	console.log(mediaRecorder.state);
	mediaRecorder.stop();
	console.log(mediaRecorder.state);
	// make blob of our data
	var file = new Blob(recordedAudio, {type: 'audio/wav'});
	const audioUrl = URL.createObjectURL(file);
	app.ports.consumeAudio.send(audioUrl)
	//const audio = new Audio(audioUrl);
	//audio.play();

})


// set up user media
function recordAudio() {
	navigator.mediaDevices.getUserMedia({
		audio: true
	})
	.then(function(stream) {
		mediaRecorder = new MediaRecorder(stream);
		// configure our chunker
		mediaRecorder.ondataavailable = handleRecordedData;
		mediaRecorder.start();
		console.log(mediaRecorder.state);
		console.log("recorder started");
	})
}


// handle recorded stream
function handleRecordedData(event) {
	if (event.data.size > 0) {
		recordedAudio.push(event.data)
	} else {
		console.log("something went saving chunks")
		console.log(event)
	}
}






