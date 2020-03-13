import { Elm } from './Main.elm'

let mediaRecorder;
let recordedAudio =  [];

const app = Elm.Main.init({ flags:  {}})

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
})


// set up user media
function recordAudio() {
	navigator.mediaDevices.getUserMedia({
		audio: true,
		video: false
	})
	.then(function(stream) {
		mediaRecorder = new MediaRecorder(stream);
		// configure our chunker
		mediaRecorder.ondataavailable = handleRecordedData;
		mediaRecorder.start();
		console.log(mediaRecorder.state);
		console.log("recorder started");

		mediaRecorder.onstop = handleBlob;
	})
}

function handleBlob(event) {
	console.log("media recorder stopped");

	var file = new Blob(recordedAudio, {type: 'audio/ogg; codecs=opus'});

	var reader = new FileReader();

	reader.onload = function() {
		console.log("recorded result read as binary ");
		var dataUrl = reader.result;
    var base64 = dataUrl.split(',')[1];
		console.log(base64);
		app.ports.consumeAudio.send(base64);
	}

	reader.readAsDataURL(file);
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






