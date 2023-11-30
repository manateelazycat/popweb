/****************************************************************************
**
** Copyright (C) 2016 The Qt Company Ltd.
** Copyright (C) 2016 Klar채lvdalens Datakonsult AB, a KDAB Group company, info@kdab.com, author Milian Wolff <milian.wolff@kdab.com>
** Contact: https://www.qt.io/licensing/
**
** This file is part of the QtWebChannel module of the Qt Toolkit.
**
** $QT_BEGIN_LICENSE:LGPL$
** Commercial License Usage
** Licensees holding valid commercial Qt licenses may use this file in
** accordance with the commercial license agreement provided with the
** Software or, alternatively, in accordance with the terms contained in
** a written agreement between you and The Qt Company. For licensing terms
** and conditions see https://www.qt.io/terms-conditions. For further
** information use the contact form at https://www.qt.io/contact-us.
**
** GNU Lesser General Public License Usage
** Alternatively, this file may be used under the terms of the GNU Lesser
** General Public License version 3 as published by the Free Software
** Foundation and appearing in the file LICENSE.LGPL3 included in the
** packaging of this file. Please review the following information to
** ensure the GNU Lesser General Public License version 3 requirements
** will be met: https://www.gnu.org/licenses/lgpl-3.0.html.
**
** GNU General Public License Usage
** Alternatively, this file may be used under the terms of the GNU
** General Public License version 2.0 or (at your option) the GNU General
** Public license version 3 or any later version approved by the KDE Free
** Qt Foundation. The licenses are as published by the Free Software
** Foundation and appearing in the file LICENSE.GPL2 and LICENSE.GPL3
** included in the packaging of this file. Please review the following
** information to ensure the GNU General Public License requirements will
** be met: https://www.gnu.org/licenses/gpl-2.0.html and
** https://www.gnu.org/licenses/gpl-3.0.html.
**
** $QT_END_LICENSE$
**
****************************************************************************/

"use strict";

var QWebChannelMessageTypes = {
    signal: 1,
    propertyUpdate: 2,
    init: 3,
    idle: 4,
    debug: 5,
    invokeMethod: 6,
    connectToSignal: 7,
    disconnectFromSignal: 8,
    setProperty: 9,
    response: 10,
};

var QWebChannel = function(transport, initCallback)
{
    if (typeof transport !== "object" || typeof transport.send !== "function") {
        console.error("The QWebChannel expects a transport object with a send function and onmessage callback property." +
                      " Given is: transport: " + typeof(transport) + ", transport.send: " + typeof(transport.send));
        return;
    }

    var channel = this;
    this.transport = transport;

    this.send = function(data)
    {
        if (typeof(data) !== "string") {
            data = JSON.stringify(data);
        }
        channel.transport.send(data);
    }

    this.transport.onmessage = function(message)
    {
        var data = message.data;
        if (typeof data === "string") {
            data = JSON.parse(data);
        }
        switch (data.type) {
            case QWebChannelMessageTypes.signal:
                channel.handleSignal(data);
                break;
            case QWebChannelMessageTypes.response:
                channel.handleResponse(data);
                break;
            case QWebChannelMessageTypes.propertyUpdate:
                channel.handlePropertyUpdate(data);
                break;
            default:
                console.error("invalid message received:", message.data);
                break;
        }
    }

    this.execCallbacks = {};
    this.execId = 0;
    this.exec = function(data, callback)
    {
        if (!callback) {
            // if no callback is given, send directly
            channel.send(data);
            return;
        }
        if (channel.execId === Number.MAX_VALUE) {
            // wrap
            channel.execId = Number.MIN_VALUE;
        }
        if (data.hasOwnProperty("id")) {
            console.error("Cannot exec message with property id: " + JSON.stringify(data));
            return;
        }
        data.id = channel.execId++;
        channel.execCallbacks[data.id] = callback;
        channel.send(data);
    };

    this.objects = {};

    this.handleSignal = function(message)
    {
        var object = channel.objects[message.object];
        if (object) {
            object.signalEmitted(message.signal, message.args);
        } else {
            console.warn("Unhandled signal: " + message.object + "::" + message.signal);
        }
    }

    this.handleResponse = function(message)
    {
        if (!message.hasOwnProperty("id")) {
            console.error("Invalid response message received: ", JSON.stringify(message));
            return;
        }
        channel.execCallbacks[message.id](message.data);
        delete channel.execCallbacks[message.id];
    }

    this.handlePropertyUpdate = function(message)
    {
        message.data.forEach(data => {
            var object = channel.objects[data.object];
            if (object) {
                object.propertyUpdate(data.signals, data.properties);
            } else {
                console.warn("Unhandled property update: " + data.object + "::" + data.signal);
            }
        });
        channel.exec({type: QWebChannelMessageTypes.idle});
    }

    this.debug = function(message)
    {
        channel.send({type: QWebChannelMessageTypes.debug, data: message});
    };

    channel.exec({type: QWebChannelMessageTypes.init}, function(data) {
        for (const objectName of Object.keys(data)) {
            new QObject(objectName, data[objectName], channel);
        }

        // now unwrap properties, which might reference other registered objects
        for (const objectName of Object.keys(channel.objects)) {
            channel.objects[objectName].unwrapProperties();
        }

        if (initCallback) {
            initCallback(channel);
        }
        channel.exec({type: QWebChannelMessageTypes.idle});
    });
};

function QObject(name, data, webChannel)
{
    this.__id__ = name;
    webChannel.objects[name] = this;

    // List of callbacks that get invoked upon signal emission
    this.__objectSignals__ = {};

    // Cache of all properties, updated when a notify signal is emitted
    this.__propertyCache__ = {};

    var object = this;

    // ----------------------------------------------------------------------

    this.unwrapQObject = function(response)
    {
        if (response instanceof Array) {
            // support list of objects
            return response.map(qobj => object.unwrapQObject(qobj))
        }
        if (!(response instanceof Object))
            return response;

        if (!response["__QObject*__"] || response.id === undefined) {
            var jObj = {};
            for (const propName of Object.keys(response)) {
                jObj[propName] = object.unwrapQObject(response[propName]);
            }
            return jObj;
        }

        var objectId = response.id;
        if (webChannel.objects[objectId])
            return webChannel.objects[objectId];

        if (!response.data) {
            console.error("Cannot unwrap unknown QObject " + objectId + " without data.");
            return;
        }

        var qObject = new QObject( objectId, response.data, webChannel );
        qObject.destroyed.connect(function() {
            if (webChannel.objects[objectId] === qObject) {
                delete webChannel.objects[objectId];
                // reset the now deleted QObject to an empty {} object
                // just assigning {} though would not have the desired effect, but the
                // below also ensures all external references will see the empty map
                // NOTE: this detour is necessary to workaround QTBUG-40021
                Object.keys(qObject).forEach(name => delete qObject[name]);
            }
        });
        // here we are already initialized, and thus must directly unwrap the properties
        qObject.unwrapProperties();
        return qObject;
    }

    this.unwrapProperties = function()
    {
        for (const propertyIdx of Object.keys(object.__propertyCache__)) {
            object.__propertyCache__[propertyIdx] = object.unwrapQObject(object.__propertyCache__[propertyIdx]);
        }
    }

    function addSignal(signalData, isPropertyNotifySignal)
    {
        var signalName = signalData[0];
        var signalIndex = signalData[1];
        object[signalName] = {
            connect: function(callback) {
                if (typeof(callback) !== "function") {
                    console.error("Bad callback given to connect to signal " + signalName);
                    return;
                }

                object.__objectSignals__[signalIndex] = object.__objectSignals__[signalIndex] || [];
                object.__objectSignals__[signalIndex].push(callback);

                // only required for "pure" signals, handled separately for properties in propertyUpdate
                if (isPropertyNotifySignal)
                    return;

                // also note that we always get notified about the destroyed signal
                if (signalName === "destroyed" || signalName === "destroyed()" || signalName === "destroyed(QObject*)")
                    return;

                // and otherwise we only need to be connected only once
                if (object.__objectSignals__[signalIndex].length == 1) {
                    webChannel.exec({
                        type: QWebChannelMessageTypes.connectToSignal,
                        object: object.__id__,
                        signal: signalIndex
                    });
                }
            },
            disconnect: function(callback) {
                if (typeof(callback) !== "function") {
                    console.error("Bad callback given to disconnect from signal " + signalName);
                    return;
                }
                object.__objectSignals__[signalIndex] = object.__objectSignals__[signalIndex] || [];
                var idx = object.__objectSignals__[signalIndex].indexOf(callback);
                if (idx === -1) {
                    console.error("Cannot find connection of signal " + signalName + " to " + callback.name);
                    return;
                }
                object.__objectSignals__[signalIndex].splice(idx, 1);
                if (!isPropertyNotifySignal && object.__objectSignals__[signalIndex].length === 0) {
                    // only required for "pure" signals, handled separately for properties in propertyUpdate
                    webChannel.exec({
                        type: QWebChannelMessageTypes.disconnectFromSignal,
                        object: object.__id__,
                        signal: signalIndex
                    });
                }
            }
        };
    }

    /**
     * Invokes all callbacks for the given signalname. Also works for property notify callbacks.
     */
    function invokeSignalCallbacks(signalName, signalArgs)
    {
        var connections = object.__objectSignals__[signalName];
        if (connections) {
            connections.forEach(function(callback) {
                callback.apply(callback, signalArgs);
            });
        }
    }

    this.propertyUpdate = function(signals, propertyMap)
    {
        // update property cache
        for (const propertyIndex of Object.keys(propertyMap)) {
            var propertyValue = propertyMap[propertyIndex];
            object.__propertyCache__[propertyIndex] = this.unwrapQObject(propertyValue);
        }

        for (const signalName of Object.keys(signals)) {
            // Invoke all callbacks, as signalEmitted() does not. This ensures the
            // property cache is updated before the callbacks are invoked.
            invokeSignalCallbacks(signalName, signals[signalName]);
        }
    }

    this.signalEmitted = function(signalName, signalArgs)
    {
        invokeSignalCallbacks(signalName, this.unwrapQObject(signalArgs));
    }

    function addMethod(methodData)
    {
        var methodName = methodData[0];
        var methodIdx = methodData[1];

        // Fully specified methods are invoked by id, others by name for host-side overload resolution
        var invokedMethod = methodName[methodName.length - 1] === ')' ? methodIdx : methodName

        object[methodName] = function() {
            var args = [];
            var callback;
            var errCallback;
            for (var i = 0; i < arguments.length; ++i) {
                var argument = arguments[i];
                if (typeof argument === "function")
                    callback = argument;
                else if (argument instanceof QObject && webChannel.objects[argument.__id__] !== undefined)
                    args.push({
                        "id": argument.__id__
                    });
                else
                    args.push(argument);
            }

            var result;
            // during test, webChannel.exec synchronously calls the callback
            // therefore, the promise must be constucted before calling
            // webChannel.exec to ensure the callback is set up
            if (!callback && (typeof(Promise) === 'function')) {
              result = new Promise(function(resolve, reject) {
                callback = resolve;
                errCallback = reject;
              });
            }

            webChannel.exec({
                "type": QWebChannelMessageTypes.invokeMethod,
                "object": object.__id__,
                "method": invokedMethod,
                "args": args
            }, function(response) {
                if (response !== undefined) {
                    var result = object.unwrapQObject(response);
                    if (callback) {
                        (callback)(result);
                    }
                } else if (errCallback) {
                  (errCallback)();
                }
            });

            return result;
        };
    }

    function bindGetterSetter(propertyInfo)
    {
        var propertyIndex = propertyInfo[0];
        var propertyName = propertyInfo[1];
        var notifySignalData = propertyInfo[2];
        // initialize property cache with current value
        // NOTE: if this is an object, it is not directly unwrapped as it might
        // reference other QObject that we do not know yet
        object.__propertyCache__[propertyIndex] = propertyInfo[3];

        if (notifySignalData) {
            if (notifySignalData[0] === 1) {
                // signal name is optimized away, reconstruct the actual name
                notifySignalData[0] = propertyName + "Changed";
            }
            addSignal(notifySignalData, true);
        }

        Object.defineProperty(object, propertyName, {
            configurable: true,
            get: function () {
                var propertyValue = object.__propertyCache__[propertyIndex];
                if (propertyValue === undefined) {
                    // This shouldn't happen
                    console.warn("Undefined value in property cache for property \"" + propertyName + "\" in object " + object.__id__);
                }

                return propertyValue;
            },
            set: function(value) {
                if (value === undefined) {
                    console.warn("Property setter for " + propertyName + " called with undefined value!");
                    return;
                }
                object.__propertyCache__[propertyIndex] = value;
                var valueToSend = value;
                if (valueToSend instanceof QObject && webChannel.objects[valueToSend.__id__] !== undefined)
                    valueToSend = { "id": valueToSend.__id__ };
                webChannel.exec({
                    "type": QWebChannelMessageTypes.setProperty,
                    "object": object.__id__,
                    "property": propertyIndex,
                    "value": valueToSend
                });
            }
        });

    }

    // ----------------------------------------------------------------------

    data.methods.forEach(addMethod);

    data.properties.forEach(bindGetterSetter);

    data.signals.forEach(function(signal) { addSignal(signal, false); });

    Object.assign(object, data.enums);
}

//required for use with nodejs
if (typeof module === 'object') {
    module.exports = {
        QWebChannel: QWebChannel
    };
}

window.clickOutsideTooltip = function(event) {
    let el = event.target;
    let selection = window.getSelection().toString();
    if (selection && selection.trim().length) {
        return;
    }
    while(el.parentNode) {
        if (el.classList.contains('rev-tooltip')) {

            let zIndex = Number(el.dataset.z);
            let others = tooltips();
            let toRemove = [];
            for (let ot of others) {
                if (Number(ot.dataset.z) > zIndex) {
                    toRemove.push(ot)
                }
            }
            for (let el of toRemove) {
                el.remove();
            }
            if (window.REV_SEARCH_SHOULD_BLUR) {
                blurLowerTooltips();
            }
            return;
        }
        el = el.parentNode;
    }
    let existingTooltips = Array.from(document.getElementsByClassName('rev-tooltip'));
    for (let el of existingTooltips) {
        el.remove();
    }

    document.removeEventListener('click', clickOutsideTooltip);

};
window.tooltips = function() {
    return document.getElementsByClassName('rev-tooltip');
};
window.maxZ = function() {
    let els = tooltips();
    let highestZ = 0;
    for (let el of els) {
        let z = Number(el.dataset.z);
        if (z > highestZ) {
            highestZ = z;
        }
    }
    return highestZ;
};
window.blurLowerTooltips = function() {
    let highestZ = maxZ();
    let els = tooltips();
    for (let el of els) {
        if (Number(el.dataset.z) < highestZ) {
            el.style.filter = 'blur(1px)';
        } else {
            el.style.filter = 'none';
        }
    }
};
window.tooltipSearch = function(tooltipId, query) {
    if (!query || !query.trim().length) {
        setTooltipSearchResults(tooltipId, []);
        return;
    }
    window.handler.expanded_on_bridge_cmd('rev-search ' + tooltipId + ' ' + query);
};
window.renderNewTooltip = function(query) {

    let sel = window.getSelection();
    if (!sel) {
        return;
    }
    window._revTooltipIdCount++;
    let id = window._revTooltipIdCount;
    let srange = sel.getRangeAt(0);
    let sbox = srange.getBoundingClientRect();

    let tooltip = document.createElement('div');
    tooltip.classList.add('rev-tooltip');
    let scroll = document.scrollingElement.scrollTop;
    let parent = srange.commonAncestorContainer;
    while(parent && (!parent.classList || !parent.classList.contains('rev-tooltip__scroll')) && parent.tagName !== 'body') {
        parent = parent.parentNode;
    }
    let zoom = 1.0;
    if (parent && parent.classList.contains('rev-tooltip__scroll')) {
        zoom = Number(parent.dataset.zoom||"1.0");
    }
    tooltip.style.left = zoom * sbox.left + 'px';
    let spaceOnBottom = window.innerHeight - sbox.bottom;
    let spaceOnTop = sbox.top;
    if (spaceOnBottom < spaceOnTop) {
        // place tooltip above selection
        tooltip.style.bottom = (window.innerHeight - (sbox.top * zoom) - scroll) + 'px';
    } else {
        // place tooltip under selection
        tooltip.style.top = (sbox.bottom * zoom + scroll) + 'px';
    }
    tooltip.id = 'rev-tooltip_' + id;


    let loader = query.length < REV_SEARCH_MAX_QUERY_LENGTH ? "Searching ..." : "Sorry, query is too long.";
    tooltip.innerHTML = `<div class='rev-tooltip__search'><svg class="rev-tooltip__search_icn" onclick="window.handler.expanded_on_bridge_cmd('rev-tt-dictionary ${query}')" xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink" width="24px" height="24px" viewBox="0 0 24 24" version="1.1"><!-- Uploaded to: SVG Repo, www.svgrepo.com, Generator: SVG Repo Mixer Tools --><path d="M17.5,12 C20.5376,12 23,14.4624 23,17.5 C23,20.5376 20.5376,23 17.5,23 C14.4624,23 12,20.5376 12,17.5 C12,14.4624 14.4624,12 17.5,12 Z M17,2 C18.325472,2 19.4100378,3.03153766 19.4946823,4.33562452 L19.5,4.5 L19.5,11.3135 C18.8699,11.11 18.1978,11 17.5,11 C13.9101,11 11,13.9101 11,17.5 C11,18.0815 11.0763889,18.6451528 11.2196181,19.1815255 L11.3135,19.5 L4.5,19.5 C4.5,20.01285 4.88604429,20.4355092 5.38337975,20.4932725 L5.5,20.5 L11.7322,20.5 C11.9715333,20.9591667 12.2640611,21.3861806 12.6016815,21.7728819 L12.8096,22 L5.5,22 C4.1745184,22 3.08996147,20.9684531 3.00531769,19.6643744 L3,19.5 L3,4.5 C3,3.1745184 4.03153766,2.08996147 5.33562452,2.00531769 L5.5,2 L17,2 Z M17.5,13.9993 C17.2545778,13.9993 17.0504,14.1761296 17.0080571,14.4094092 L17,14.4993 L17.0005,17 L14.4961,17 C14.22,17 13.9961,17.2239 13.9961,17.5 C13.9961,17.7455111 14.1730086,17.9496198 14.4062355,17.9919462 L14.4961,18 L17.0006,18 L17.0011,20.5035 C17.0011,20.7797 17.225,21.0035 17.5011,21.0035 C17.7466111,21.0035 17.9507198,20.8266704 17.9930462,20.5933908 L18.0011,20.5035 L18.0006,18 L20.503,18 C20.7792,18 21.003,17.7762 21.003,17.5 C21.003,17.2545778 20.8261704,17.0504 20.5928908,17.0080571 L20.503,17 L18.0005,17 L18,14.4993 C18,14.2231 17.7761,13.9993 17.5,13.9993 Z M15,5 L7,5 C6.44772,5 6,5.44772 6,6 L6,7 C6,7.55228 6.44772,8 7,8 L15,8 C15.5523,8 16,7.55228 16,7 L16,6 C16,5.44772 15.5523,5 15,5 Z" id="🎨-Color"></path></svg>
                            <input type='text' id='rev-tooltip__search_${id}'
                                onkeyup='tooltipSearch(${id}, this.value)' onmouseup='event.preventDefault(); event.stopPropagation();' onclick='event.preventDefault(); event.stopPropagation();' placeholder='Search'/>
                        </div>
                        <div class='rev-tooltip__scroll' id='rev-tooltip__scroll_${id}'>${loader}</div>
                        <div class='rev-tooltip__bottom' onmouseup='onTooltipBottomBarMouseup(event)'>
                            <svg class='rev-tooltip__zoom' onclick='tooltipZoomIn(event, ${id})' width="15" height="15" viewBox="0 0 24 24" fill="none" > <path fill-rule="evenodd" clip-rule="evenodd" d="M15.3431 15.2426C17.6863 12.8995 17.6863 9.1005 15.3431 6.75736C13 4.41421 9.20101 4.41421 6.85786 6.75736C4.51472 9.1005 4.51472 12.8995 6.85786 15.2426C9.20101 17.5858 13 17.5858 15.3431 15.2426ZM16.7574 5.34315C19.6425 8.22833 19.8633 12.769 17.4195 15.9075C17.4348 15.921 17.4498 15.9351 17.4645 15.9497L21.7071 20.1924C22.0976 20.5829 22.0976 21.2161 21.7071 21.6066C21.3166 21.9971 20.6834 21.9971 20.2929 21.6066L16.0503 17.364C16.0356 17.3493 16.0215 17.3343 16.008 17.319C12.8695 19.7628 8.32883 19.542 5.44365 16.6569C2.31946 13.5327 2.31946 8.46734 5.44365 5.34315C8.56785 2.21895 13.6332 2.21895 16.7574 5.34315ZM10.1005 7H12.1005V10H15.1005V12H12.1005V15H10.1005V12H7.10052V10H10.1005V7Z" fill="currentColor" /> </svg>
                            <svg class='rev-tooltip__zoom' onclick='tooltipZoomOut(event, ${id})'  width="15" height="15" viewBox="0 0 24 24" fill="none" > <path fill-rule="evenodd" clip-rule="evenodd" d="M15.3431 15.2426C17.6863 12.8995 17.6863 9.1005 15.3431 6.75736C13 4.41421 9.20101 4.41421 6.85786 6.75736C4.51472 9.1005 4.51472 12.8995 6.85786 15.2426C9.20101 17.5858 13 17.5858 15.3431 15.2426ZM16.7574 5.34315C19.6425 8.22833 19.8633 12.769 17.4195 15.9075C17.4348 15.921 17.4498 15.9351 17.4645 15.9497L21.7071 20.1924C22.0976 20.5829 22.0976 21.2161 21.7071 21.6066C21.3166 21.9971 20.6834 21.9971 20.2929 21.6066L16.0503 17.364C16.0356 17.3493 16.0215 17.3343 16.008 17.319C12.8695 19.7628 8.32883 19.542 5.44365 16.6569C2.31946 13.5327 2.31946 8.46734 5.44365 5.34315C8.56785 2.21895 13.6332 2.21895 16.7574 5.34315ZM7.10052 10V12H15.1005V10L7.10052 10Z" fill="currentColor" /> </svg>
                            <svg class='rev-tooltip__resize' onmousedown='initResize(event, ${id})' width="15" height="15" viewBox="0 0 24 24" fill="none"> <path d="M10.1005 2.10052V4.10052H5.51471L11.293 9.87878L9.87875 11.293L4.10046 5.51471L4.10046 10.1005H2.10046L2.10046 2.10052H10.1005Z" fill="currentColor" /> <path d="M21.8995 13.8995H19.8995V18.4853L14.1212 12.707L12.707 14.1213L18.4853 19.8995H13.8995V21.8995H21.8995V13.8995Z" fill="currentColor" /> <path d="M16.2426 9.1716L14.8284 7.75739L7.7573 14.8285L9.17151 16.2427L16.2426 9.1716Z" fill="currentColor" /></svg>
                        </div>
                        `;
    let highestZ = maxZ();
    tooltip.style.zIndex = highestZ + 1;
    tooltip.dataset.z = highestZ + 1;

    document.body.appendChild(tooltip);
    if (highestZ > 0 && REV_SEARCH_SHOULD_BLUR) {
        blurLowerTooltips();
    }
    if (query.length < REV_SEARCH_MAX_QUERY_LENGTH) {
        let afterRender = () => {
            let searchInp = document.getElementById('rev-tooltip__search_'+ id);
            if (!searchInp) {
                setTimeout(afterRender, 20);
                return;
            }
            searchInp.value = query;
            tooltipSearch(id, query);

        }
        afterRender();
    }

    setTimeout(() => {
        document.addEventListener('click', clickOutsideTooltip);
    }, 100);

};
window.setTooltipSearchResults = function(tooltipId, results) {

    let tooltip_scroll = document.getElementById('rev-tooltip__scroll_' + tooltipId);
    // to prevent jumping
    tooltip_scroll.style.minHeight = tooltip_scroll.offsetHeight + 'px';
    let html = '';
    if (!results || !results.length) {
        html = '<center class="no-results">Sorry, found no search results.</center>'
    } else {
        for (let r of results){
            html += `<div class='sr'><svg class="rev-tooltip__playsound" onclick="window.handler.expanded_on_bridge_cmd('rev-tt-playsound ${r[0]}')"  width="24px" height="24px" viewBox="0 0 24 24" fill="none" xmlns="http://www.w3.org/2000/svg"><path d="M3 16V8H6L11 4V20L6 16H3Z" stroke="#000000" stroke-linecap="round" stroke-linejoin="round"/><path d="M13 9C13 9 15 9.5 15 12C15 14.5 13 15 13 15" stroke="#000000" stroke-linecap="round" stroke-linejoin="round"/><path d="M15 7C15 7 18 7.83333 18 12C18 16.1667 15 17 15 17" stroke="#000000" stroke-linecap="round" stroke-linejoin="round"/><path d="M17 5C17 5 21 6.16667 21 12C21 17.8333 17 19 17 19" stroke="#000000" stroke-linecap="round" stroke-linejoin="round"/></svg>${r[1]}<svg class="rev-tooltip__edit" onclick="window.handler.expanded_on_bridge_cmd('rev-tt-edit ${r[0]}')" width="14" height="14" viewBox="0 0 512 512"><path d="M362.7 19.32C387.7-5.678 428.3-5.678 453.3 19.32L492.7 58.75C517.7 83.74 517.7 124.3 492.7 149.3L444.3 197.7L314.3 67.72L362.7 19.32zM421.7 220.3L188.5 453.4C178.1 463.8 165.2 471.5 151.1 475.6L30.77 511C22.35 513.5 13.24 511.2 7.03 504.1C.8198 498.8-1.502 489.7 .976 481.2L36.37 360.9C40.53 346.8 48.16 333.9 58.57 323.5L291.7 90.34L421.7 220.3z"/></div>`;
        }
    }
    tooltip_scroll.innerHTML = html;
    tooltip_scroll.scrollTop = 0;
    setTimeout(function(){
        let tooltip = document.getElementById('rev-tooltip_' + tooltipId);
        let sbox = tooltip.getBoundingClientRect();
        if (sbox.top < 0) {
            tooltip.style.maxHeight = Math.max(100, tooltip.offsetHeight + sbox.top) + 'px';
        }
        if (typeof(window.MathJax) !== 'undefined' && window.MathJax.typeset) {
            window.MathJax.typeset();
        }
    }, 50);

};
if (typeof(window._revTooltipIdCount) === 'undefined') {

    window._revTooltipIdCount = 1;

    document.addEventListener('mouseup', (event) => {
        let selection = window.getSelection().toString().replace(/\n|\s{2,}/g, ' ');
        if (selection && selection.trim().length) {
            renderNewTooltip(selection);
        }
    });
}

window.onTooltipBottomBarMouseup = function(e) {
    if (_tooltipResize.resizing) {
        return;
    }
    e.stopPropagation();
}

/* zooming */

window.tooltipZoomOut = function(event, id) {
    let tooltipScrollBody = document.getElementById('rev-tooltip__scroll_'+id);
    if (!tooltipScrollBody.dataset.zoom) {
        tooltipScrollBody.dataset.zoom = "1";
    }
    let currentZoomLevel = Number(tooltipScrollBody.dataset.zoom)
    let newZoomLevel = currentZoomLevel > 0.3 ? currentZoomLevel - 0.1 : currentZoomLevel;
    tooltipScrollBody.style.zoom = newZoomLevel;
    tooltipScrollBody.dataset.zoom = newZoomLevel.toString();
}
window.tooltipZoomIn = function(event, id) {
    let tooltipScrollBody = document.getElementById('rev-tooltip__scroll_'+id);
    if (!tooltipScrollBody.dataset.zoom) {
        tooltipScrollBody.dataset.zoom = "1";
    }
    let currentZoomLevel = Number(tooltipScrollBody.dataset.zoom)
    let newZoomLevel = currentZoomLevel < 2.0 ? currentZoomLevel + 0.1 : currentZoomLevel;
    tooltipScrollBody.style.zoom = newZoomLevel;
    tooltipScrollBody.dataset.zoom = newZoomLevel.toString();
}


/* resizing */

window._tooltipResize = {
    el: null,
    startX: 0,
    startY: 0,
    startWidth: 0,
    startHeight: 0,
    resizing: false
};
window.initResize = function(e, id) {
    e.preventDefault();
    let r = window._tooltipResize;
    r.resizing = true;
    let tooltip = document.getElementById('rev-tooltip_'+id);
    r.el = tooltip;
    if (r.el.style.bottom && r.el.style.bottom.length) {
        r.el.style.top = r.el.getBoundingClientRect().top + 'px';
        r.el.style.bottom = null;
    }
    r.startX = e.clientX;
    r.startY = e.clientY;
    r.startWidth = parseInt(document.defaultView.getComputedStyle(tooltip).width, 10);
    r.startHeight = parseInt(document.defaultView.getComputedStyle(tooltip).height, 10);
    document.documentElement.addEventListener('mousemove', doResizeDrag, false);
    document.documentElement.addEventListener('mouseup', stopResizeDrag, false);
}
window.doResizeDrag = function(e) {
    e.preventDefault();
    e.stopPropagation();
    let r = window._tooltipResize;
    r.el.style.width = (r.startWidth + e.clientX - r.startX) + 'px';
    r.el.style.height = (r.startHeight + e.clientY - r.startY) + 'px';
    r.el.style.maxWidth = r.el.style.width;
    r.el.style.maxHeight = r.el.style.height;
}
window.stopResizeDrag = function(e) {
    e.preventDefault();
    e.stopPropagation();
    document.documentElement.removeEventListener('mousemove', doResizeDrag, false);
    document.documentElement.removeEventListener('mouseup', stopResizeDrag, false);
    _tooltipResize.resizing = false;
}
