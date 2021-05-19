
customElements.define('leaflet-map',
class extends HTMLElement { 
    // things required by Custom Elements
    constructor() { super(); }
    connectedCallback() { this.initMap(); }
    attributeChangedCallback() { this.updateAttrs(); }
    static get observedAttributes() { 
        return ['latitude', 'longitude', 'zoom']; 
    }
    
    observe(options, element) {
        var observer = new MutationObserver ((mutations) => this.childObserver(mutations));
        observer.observe(element, options);
    }

    childObserver(mutations) {
        for (const mutation of mutations) {
            if (mutation.type === 'childList') {
                // start observing new markers
                Array.from(this.childNodes)
                    .filter(node => node.nodeName === "LEAFLET-MARKER")
                    .forEach(node => this.observe ({ attributes: true }, node))

                

                this.showMarkers(this.childNodes);
            } else if (mutation.type === 'attributes') {
                this.showMarkers(this.childNodes);
            }
        }
    }

    initMap() {
        this.observe({childList: true}, this);

        Array.from(this.childNodes)
            .filter(node => node.nodeName === "LEAFLET-MARKER")
            .forEach(node => this.observe ({ attributes: true }, node))

        if (this.isConnected) {
            var latitude = this.getAttribute('latitude');
            var longitude = this.getAttribute('longitude');
            var zoom = this.getAttribute('zoom');
            
            this.markersLayer = L.layerGroup();
            if (this.mymap == undefined) {
                console.log("init map");

                this.mymap = 
                    L.map(this, { layers: [this.markersLayer] })
                        .setView([latitude, longitude], zoom);
                
                this.showMarkers(this.childNodes);

                L.tileLayer('http://{s}.tile.osm.org/{z}/{x}/{y}.png', {
                    attribution: '&copy; <a href="http://osm.org/copyright">OpenStreetMap</a> contributors',
                }).addTo(this.mymap);
            
            }
        }
    }

    showMarkers(markers) {
        this.markersLayer.clearLayers();
        for (const marker of markers) {
            if (marker.nodeName === "LEAFLET-MARKER") {
                var latitude = marker.getAttribute("latitude");
                var longitude = marker.getAttribute("longitude");
                var html = marker.getAttribute("html");
                var width = marker.getAttribute("icon-width");
                var height = marker.getAttribute("icon-height");
                var className = marker.getAttribute("className");

                var icon = L.divIcon({
                    html: html,
                    iconSize: [width, height],
                    className: className
                });

                L.marker([latitude, longitude])
                    .setIcon(icon)
                    .addTo(this.markersLayer);
            }
        }
    }

    updateAttrs() {
        var latitude = this.getAttribute('latitude');
        var longitude = this.getAttribute('longitude');
        var zoom = this.getAttribute('zoom');

        if (this.mymap !== undefined) {
            this.mymap.setView([latitude, longitude], zoom);
            console.log("updated");
        }
    }
}
);