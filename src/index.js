import { Elm } from './App/Main.elm';
import registerServiceWorker from './registerServiceWorker';

const basePath = new URL(document.baseURI).pathname;

let app = Elm.App.Main.init({
  node: document.getElementById('root'),
  flags : { basePath }
});

app.ports.requestInstances.subscribe(function ( message ) {
   let pricing = new AWS.Pricing({
      region: message[0],
      apiVersion: '2017-10-15',
      accessKeyId: process.env.ELM_APP_ACCESS_KEY_ID,
      secretAccessKey: process.env.ELM_APP_SERCRET_ACCESS_KEY
   });
   let nextToken = message[1];
   let maxResults = message[2];
   let params = { Filters: [
         {
        Field: 'ServiceCode',
        Type: 'TERM_MATCH',
        Value: 'AmazonEC2'
      }
    ],
    ServiceCode: 'AmazonEC2',
    FormatVersion: 'aws_v1',
    MaxResults: maxResults,
    NextToken: nextToken
  };
  pricing.getProducts(params, function (err, data) {
    if (err) {
      console.log(err);
    } else { 
      app.ports.receiveInstances.send(JSON.stringify(data));  // successful response -- DECIDE: send back string JSON or just object?
    }
  });
});

registerServiceWorker();
