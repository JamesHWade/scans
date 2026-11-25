// Browser-side behaviour for the scans app: selection highlight without a
// list re-render, keyboard navigation, and the tool expand/collapse toggle.
(function () {
  var state = { selected: null, pendingEvidence: null, returnTo: null };

  function setMobilePane(id, open) {
    if (!window.matchMedia('(max-width: 1199px)').matches) return;
    var pane = document.getElementById(id);
    var toggle = pane && pane.parentElement.querySelector(':scope > .collapse-toggle');
    if (toggle && (toggle.getAttribute('aria-expanded') === 'true') !== open) toggle.click();
  }

  function revealEvidence() {
    var pending = state.pendingEvidence;
    if (!pending) return;
    var target = document.getElementById(pending.id);
    if (!target) return;
    document.querySelectorAll('.scans-app-evidence-active').forEach(function (node) {
      node.classList.remove('scans-app-evidence-active');
    });
    var node = target;
    while (node) {
      if (node.tagName === 'DETAILS') node.open = true;
      node = node.parentElement;
    }
    if (!target.getClientRects().length) return;
    setMobilePane('scans_app_browser_pane', false);
    setMobilePane('scans_app_investigation_pane', false);
    target.classList.add('scans-app-evidence-active');
    target.focus({ preventScroll: true });
    target.scrollIntoView({ block: 'nearest', behavior: 'instant' });
    if (state.returnTo && document.contains(state.returnTo)) {
      var back = document.getElementById('scans-app-return-to-evidence');
      if (back) back.remove();
      back = document.createElement('button');
      back.id = 'scans-app-return-to-evidence';
      back.type = 'button';
      back.className = 'btn btn-sm btn-link';
      back.textContent = 'Return to finding';
      back.addEventListener('click', function () {
        setMobilePane('scans_app_investigation_pane', true);
        state.returnTo.focus();
        state.returnTo.scrollIntoView({ block: 'nearest' });
      });
      target.appendChild(back);
    }
    var base = window.location.hash.split('?event=')[0];
    if (base && base.indexOf('/') !== -1 && pending.event_id) {
      window.history.replaceState(null, '', base + '?event=' + encodeURIComponent(pending.event_id));
    }
    state.pendingEvidence = null;
  }

  function applySelection() {
    var entries = document.querySelectorAll(".scans-app-entry");
    var current = null;
    entries.forEach(function (entry) {
      var on = state.selected !== null && entry.id === state.selected;
      entry.classList.toggle("scans-app-entry-selected", on);
      if (on) {
        entry.setAttribute("aria-current", "true");
        current = entry;
      } else {
        entry.removeAttribute("aria-current");
      }
    });
    // Scroll the list only; scrollIntoView would also drag the filters
    // above it out of view.
    var list = current && current.closest(".scans-app-browser-entries");
    if (list) {
      var top = current.offsetTop - list.offsetTop;
      var bottom = top + current.offsetHeight;
      if (top < list.scrollTop) {
        list.scrollTop = top - 8;
      } else if (bottom > list.scrollTop + list.clientHeight) {
        list.scrollTop = bottom - list.clientHeight + 8;
      }
    }
  }

  // Copy an attribute's text or a text box's value. When the browser refuses
  // clipboard access the text is shown selected so it can be copied by hand.
  function copyText(button) {
    var box = button.dataset.scansCopyFrom && document.getElementById(button.dataset.scansCopyFrom);
    var text = box ? box.value : button.dataset.scansCopy;
    var status = (button.closest(".modal-content") || document).querySelector(".scans-app-copy-status");
    function say(message) {
      if (status) status.textContent = message;
      else {
        var label = button.textContent;
        button.textContent = message;
        setTimeout(function () { button.textContent = label; }, 1600);
      }
    }
    function byHand() {
      if (!box) {
        box = document.createElement("input");
        box.className = "form-control form-control-sm scans-app-copy-fallback";
        box.readOnly = true;
        box.value = text;
        box.setAttribute("aria-label", "Link to copy");
        button.insertAdjacentElement("afterend", box);
      }
      box.focus();
      box.select();
      say("Press Ctrl or Cmd + C to copy");
    }
    if (navigator.clipboard && window.isSecureContext) {
      navigator.clipboard.writeText(text).then(function () { say("Copied"); }, byHand);
    } else {
      byHand();
    }
  }

  function isTyping(target) {
    if (!target) return false;
    var tag = (target.tagName || "").toLowerCase();
    return (
      tag === "input" ||
      tag === "textarea" ||
      tag === "select" ||
      target.isContentEditable
    );
  }

  function ready(fn) {
    if (document.readyState !== "loading") fn();
    else document.addEventListener("DOMContentLoaded", fn);
  }

  ready(function () {
    if (!window.Shiny) return;
    var smallScreen = window.matchMedia('(max-width: 1199px)');
    function reportScreenSize() {
      Shiny.setInputValue('scans_app_small_screen', smallScreen.matches);
    }
    $(document).on('shiny:connected', reportScreenSize);
    smallScreen.addEventListener('change', reportScreenSize);
    ['scans_app_browser_pane', 'scans_app_investigation_pane'].forEach(function (id) {
      var pane = document.getElementById(id);
      var toggle = pane && pane.parentElement.querySelector(':scope > .collapse-toggle');
      if (!toggle) return;
      var label = id === 'scans_app_browser_pane' ? 'Trajectories' :
        (document.getElementById('scans_app_ask_scope') ? 'Findings / Ask' : 'Findings');
      toggle.setAttribute('aria-label', 'Toggle ' + label.toLowerCase());
      toggle.setAttribute('title', label);
      toggle.dataset.scansLabel = label;
    });

    Shiny.addCustomMessageHandler("scans-app-select", function (message) {
      state.selected = message && message.id ? message.id : null;
      applySelection();
      if (state.selected) setMobilePane('scans_app_browser_pane', false);
      // Keep the URL pointing at the selected trajectory so it can be shared.
      var hash = message && message.hash ? "#" + message.hash : "";
      if (hash && window.location.hash.split('?event=')[0] === hash) {
        hash = window.location.hash;
      }
      if (hash !== window.location.hash && window.history.replaceState) {
        window.history.replaceState(
          null,
          "",
          window.location.pathname + window.location.search + hash
        );
      }
      setTimeout(revealEvidence, 0);
    });

    Shiny.addCustomMessageHandler('scans-app-reveal', function (message) {
      state.pendingEvidence = message;
      state.returnTo = null;
      setTimeout(revealEvidence, 0);
    });
    Shiny.addCustomMessageHandler('scans-app-evidence-unavailable', function (message) {
      document.querySelectorAll('a[href^="#scans-evidence="]').forEach(function (link) {
        if (link.getAttribute('href') === '#scans-evidence=' + message.key) {
          link.classList.add('scans-app-unsupported-evidence');
          link.title = 'Unsupported evidence reference';
        }
      });
    });

    // The server is busy while it reads a source, so the page itself says
    // what is being read; the banner clears when the server is idle again.
    function showLoading(label) {
      var banner = document.querySelector(".scans-app-loading");
      if (!banner) return;
      var texts = banner.querySelectorAll("template[data-scans-loading-for]");
      var text = Array.prototype.find.call(texts, function (node) {
        return node.dataset.scansLoadingFor === label;
      });
      if (text) banner.querySelector(".scans-app-loading-text").textContent = text.innerHTML;
      document.body.setAttribute("data-scans-loading", "");
    }
    $(document).on("shiny:inputchanged", function (event) {
      if (event.name === "scans_app_application") showLoading(event.value);
      if (event.name === "scans_app_reload") {
        var select = document.getElementById("scans_app_application");
        showLoading(select ? select.value : null);
      }
    });
    $(document).on("shiny:idle", function () {
      document.body.removeAttribute("data-scans-loading");
    });

    function sendHash() {
      var hash = window.location.hash;
      if (!hash || hash.length < 2) return;
      Shiny.setInputValue("scans_app_hash", hash.slice(1), { priority: "event" });
    }
    $(document).one("shiny:connected", sendHash);
    window.addEventListener("hashchange", sendHash);

    // The entries output re-renders when the filters change; re-apply the
    // highlight once the new list is in the DOM.
    $(document).on("shiny:value", function (event) {
      if (event.name === "scans_app_entries") {
        setTimeout(applySelection, 0);
      }
      if (event.name === 'scans_app_transcript') setTimeout(revealEvidence, 0);
    });
    $(document).on('shown.bs.tab', function () { setTimeout(revealEvidence, 0); });

    document.addEventListener("keydown", function (event) {
      if (event.defaultPrevented || event.altKey || event.ctrlKey || event.metaKey) {
        return;
      }
      if (isTyping(event.target)) return;
      var bar = event.target.closest && event.target.closest(".scans-app-bar");
      if (bar && (event.key === "ArrowLeft" || event.key === "ArrowRight")) {
        var step = event.key === "ArrowLeft" ? "previousElementSibling" : "nextElementSibling";
        var next = bar[step];
        while (next && (next.disabled || !next.classList.contains("scans-app-bar"))) next = next[step];
        if (next) next.focus();
        event.preventDefault();
        return;
      }
      // Review shortcuts are declared on the controls they trigger.
      if (!event.repeat) {
        var key = event.key.length === 1 ? event.key.toLowerCase() : null;
        var control = key && Array.prototype.find.call(
          document.querySelectorAll("[data-scans-key]"),
          function (node) { return node.dataset.scansKey === key && node.getClientRects().length; }
        );
        if (control) {
          event.preventDefault();
          control.click();
          return;
        }
      }
      var direction = null;
      if (event.key === "ArrowDown" || event.key === "j") direction = "next";
      if (event.key === "ArrowUp" || event.key === "k") direction = "prev";
      if (!direction) return;
      event.preventDefault();
      Shiny.setInputValue(
        "scans_app_nav",
        { direction: direction, nonce: Date.now() },
        { priority: "event" }
      );
    });

    document.addEventListener("click", function (event) {
      var link = event.target.closest('a');
      if (link && (link.getAttribute('href') || '').indexOf('#scans-evidence=') === 0) {
        event.preventDefault();
        Shiny.setInputValue('scans_app_chat_evidence', link.getAttribute('href').slice(16), {priority:'event'});
        return;
      }
      var evidence = event.target.closest('[data-scans-event]');
      if (evidence) {
        event.preventDefault();
        state.returnTo = evidence;
        state.pendingEvidence = { id: evidence.getAttribute('href').slice(1), event_id: evidence.dataset.scansEvent };
        revealEvidence();
        return;
      }
      // The innermost facet wins, so a failed-calls cell inside a tool row
      // applies its own, narrower filter.
      var copy = event.target.closest("[data-scans-copy], [data-scans-copy-from]");
      if (copy) {
        copyText(copy);
        return;
      }
      var disposition = event.target.closest("[data-scans-disposition]");
      if (disposition) {
        var bar = disposition.closest("[data-scans-target]");
        Shiny.setInputValue(
          "scans_app_disposition",
          {
            label: disposition.dataset.scansDisposition,
            trajectory_id: bar ? bar.dataset.scansTarget : null,
            nonce: Date.now()
          },
          { priority: "event" }
        );
        return;
      }
      var expand = event.target.closest("[data-scans-expand]");
      if (expand) {
        var open = expand.getAttribute("aria-expanded") !== "true";
        expand.setAttribute("aria-expanded", open ? "true" : "false");
        expand.parentElement.classList.toggle("scans-app-expanded", open);
        return;
      }
      var clear = event.target.closest("[data-scans-facet-clear]");
      if (clear) {
        Shiny.setInputValue(
          "scans_app_facet_clear",
          { facet: clear.dataset.scansFacetClear, nonce: Date.now() },
          { priority: "event" }
        );
        return;
      }
      var facet = event.target.closest("[data-scans-facet]");
      if (facet) {
        Shiny.setInputValue(
          "scans_app_facet",
          {
            facet: facet.dataset.scansFacet,
            value: facet.hasAttribute("data-scans-value") ? facet.dataset.scansValue : null
          },
          { priority: "event" }
        );
        return;
      }
      var pattern = event.target.closest("[data-scans-pattern]");
      if (pattern) {
        Shiny.setInputValue("scans_app_pattern", pattern.dataset.scansPattern, {
          priority: "event"
        });
        return;
      }
      var trajectory = event.target.closest("[data-scans-trajectory]");
      if (trajectory) {
        Shiny.setInputValue("scans_app_inspect", trajectory.dataset.scansTrajectory, {
          priority: "event"
        });
        return;
      }
      var button = event.target.closest(
        "#scans_app_tools_open, #scans_app_tools_close"
      );
      if (!button) return;
      var open = button.id === "scans_app_tools_open";
      document
        .querySelectorAll(".scans-app-transcript details.scans-app-tool")
        .forEach(function (details) {
          details.open = open;
        });
    });
  });
})();
