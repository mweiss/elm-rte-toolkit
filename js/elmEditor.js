

const getSelectionPath = (node, editor, offset) => {
    let originalNode = node;
    if (!node) {
        return null;
    }

    let path = [];
    while (node && node.tagName !== "BODY") {
        path.push(node);
        if (node === editor) {
            break;
        }
        node = node.parentNode
    }

    if (path[path.length - 1] !== editor) {
        return null
    }

    let indexPath = [];
    for (let i = 0; i < path.length - 1; i += 1) {
        let child = path[i];
        let parent = path[i + 1];

        let index = 0;
        for (let childNode of parent.childNodes) {
            if (childNode === child) {
                break;
            }
            index += 1;
        }
        indexPath.unshift(index);
    }

    if (originalNode.nodeType === Node.ELEMENT_NODE && offset > 0) {
        indexPath.push(offset - 1)
    }

    if (indexPath.length <= 2) {
        return null;
    }

    // TODO: clean this up Drop the first two nodes
    indexPath.shift();
    indexPath.shift();

    return indexPath.slice();
};

const findNodeFromPath = (path, editor) => {
    if (!path) {
        return null;
    }

    if (typeof path === "string") {
        path = path.split(":").map((v) => Number(v));
    }

    let node = editor;
    let newPath = [0, 0, ...path];
    while (newPath.length && node) {
        let index = newPath.shift();
        node = node.childNodes && node.childNodes[index];
    }

    return node || null;
};


let adjustOffsetReverse = (node, offset) => {
    if (node.nodeType === Node.TEXT_NODE && node.nodeValue === "\u200B") {
        return 1;
    }
    if (node.nodeType === Node.TEXT_NODE && offset > node.nodeValue.length) {
        return node.nodeValue.length;
    }
    return offset;
};

let adjustOffset = (node, offset) => {
    if ((node.nodeType === Node.TEXT_NODE && node.nodeValue === "\u200B")
        || node.nodeType === Node.ELEMENT_NODE) {
        return 0;
    }

    return offset;
};

class SelectionState extends HTMLElement {
    static get observedAttributes() {
        return ["selection"];
    }

    constructor() {
        super();
        this.selectionChange = this.selectionChange.bind(this);
    }

    attributeChangedCallback(name, oldValue, newValue) {
        if (name !== "selection") {
            return;
        }
        let selectionObj = {};
        for (let pair of newValue.split(",")) {
            let splitPair = pair.split("=");
            if (splitPair.length === 2) {
                selectionObj[splitPair[0]] = splitPair[1]
            }
        }

        let focusOffset = Number(selectionObj["focus-offset"]);
        const focusNode = this.findNodeFromPath(selectionObj["focus-node"]);
        let anchorOffset = Number(selectionObj["anchor-offset"]);
        const anchorNode = this.findNodeFromPath(selectionObj["anchor-node"]);

        if (focusNode && anchorNode) {
            const sel = document.getSelection();

            anchorOffset = adjustOffsetReverse(anchorNode, anchorOffset);
            focusOffset = adjustOffsetReverse(focusNode, focusOffset);
            try {
                sel.setBaseAndExtent(anchorNode, anchorOffset, focusNode, focusOffset);
            } catch (e) {
                console.log("Uh oh, the selection state was incorrect!" +
                    "This maybe happens because attributes are stale on the web component?", focusOffset, focusNode, anchorOffset, anchorNode);
            }
        }
    }

    connectedCallback() {
        document.addEventListener("selectionchange", this.selectionChange)
    }

    disconnectedCallback() {
        document.removeEventListener("selectionchange", this.selectionChange)
    }

    getSelectionPath(node, offset) {
        return getSelectionPath(node, this.parentNode, offset)
    }

    findNodeFromPath(path) {
        return findNodeFromPath(path, this.parentNode)
    }

    getSelectionObject() {
        let selection = {"selectionExists": false};
        let selectionObj = getSelection();
        if (!selectionObj) {
            return selection
        }
        let anchorPath = this.getSelectionPath(selectionObj.anchorNode, selectionObj.anchorOffset);
        let focusPath = this.getSelectionPath(selectionObj.focusNode, selectionObj.focusOffset);
        if (!anchorPath || !focusPath) {
            return selection
        }
        let anchorOffset = adjustOffset(selectionObj.anchorNode, selectionObj.anchorOffset);
        let focusOffset = adjustOffset(selectionObj.focusNode, selectionObj.focusOffset);
        return {
            "selectionExists": true,
            "anchorOffset": anchorOffset,
            "focusOffset": focusOffset,
            "anchorNode": anchorPath,
            "focusNode": focusPath,
        }
    }

    selectionChange(e) {
        let selection = this.getSelectionObject(e);

        if (!selection.selectionExists) {
            return;
        }
        let event = new CustomEvent("editorselectionchange", { detail: selection });
        this.parentNode.dispatchEvent(event);
    };
}

class ElmEditor extends HTMLElement {
    constructor() {
        super();
        this.mutationObserverCallback = this.mutationObserverCallback.bind(this);
        this._observer = new MutationObserver(this.mutationObserverCallback);
    }

    connectedCallback() {
        this._observer.observe(this,  { characterDataOldValue: true, attributeOldValue: false, attributes: false, childList: true, subtree: true, characterData: true });
    }

    disconnectedCallback() {
        this._observer.disconnect();
    }

    mutationObserverCallback(mutationsList, observer) {
        let element = this.querySelector('[data-rte-main="true"]');
        let selection = this.childNodes[1].getSelectionObject();

        let event = new CustomEvent("editorchange", {
            detail: {
                root: element,
                selection: selection
            }
        });
        this.dispatchEvent(event);
    };
}

customElements.define('elm-editor', ElmEditor);
customElements.define('selection-state', SelectionState);