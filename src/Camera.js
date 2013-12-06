/**
 *
 * ## Example
 *
 * You can use the {@link Ext.space.Camera#capture} function to take a photo:
 *
 *     var promise = Ext.space.Camera.capture({
 *         quality: 75,
 *         width: 200,
 *         height: 200,
 *         destination: 'file'
 *     });
 *
 *     promise.then(function(image){ 
            //either URI to the image or data URI of the selected image.
       })
 *
 * See the documentation for {@link Ext.space.Camera#capture} all available configurations.
 */
Ext.define('Ext.space.Camera', {
    singleton: true,

    source: {
        library: 0,
        camera: 1,
        album: 2
    },

    destination: {
        data: 0, // Returns base64-encoded string
        file: 1  // Returns file's URI
    },

    encoding: {
        jpeg: 0,
        jpg: 0,
        png: 1
    },

    /**
     * Allows you to capture a photo.
     *
     * @param {Object} options
     * The options to use when taking a photo.
     *
     * @param {Function} options.success
     * The success callback which is called when the photo has been taken.
     *
     * @param {String} options.success.image
     * The image which was just taken, either a base64 encoded string or a URI depending on which
     * option you chose (destination).
     *
     * @param {Function} options.failure
     * The function which is called when something goes wrong.
     *
     * @param {Object} scope
     * The scope in which to call the `success` and `failure` functions, if specified.
     *
     * @param {Number} options.quality
     * The quality of the image which is returned in the callback. This should be a percentage.
     *
     * @param {String} options.source
     * The source of where the image should be taken. Available options are:
     *
     * - **album** - prompts the user to choose an image from an album
     * - **camera** - prompts the user to take a new photo (default)
     * - **library** - prompts the user to choose an image from the library
     *
     * @param {String} destination
     * The destination of the image which is returned. Available options are:
     *
     * - **data** - returns a base64 encoded string
     * - **file** - returns the file's URI (default)
     *
     * @param {String} encoding
     * The encoding of the returned image. Available options are:
     *
     * - **jpg** (default)
     * - **png**
     *
     * @param {Number} width
     * The width of the image to return
     *
     * @param {Number} height
     * The height of the image to return
     */
    capture: function(options) {
        var sources = this.source,
            
            destinations = this.destination,
            encodings = this.encoding,

            source = options.source || "camera",
            destination = options.destination || "data",
            encoding = options.encoding || "jpg";

        var result = new Ext.Promise();

        if (sources.hasOwnProperty(source)) {
            source = sources[source];
        }

        if (destinations.hasOwnProperty(destination)) {
            destination = destinations[destination];
        }

        if (encodings.hasOwnProperty(encoding)) {
            encoding = encodings[encoding];
        }

        Ext.space.Communicator.send({
            command: 'Camera#capture',
            callbacks: {
                success: function(image){
                    result.fulfill(image);
                },
                failure: function(error){
                    result.reject(error);
                },
            },
            scope: options.scope,
            quality: options.quality,
            width: options.width,
            height: options.height,
            source: source,
            destination: destination,
            encoding: encoding
        });
        return result;
    }
});
