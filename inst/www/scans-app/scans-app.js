// Browser-side behaviour for the scans app: selection highlight without a
// list re-render, keyboard navigation, and the tool expand/collapse toggle.
(function () {
  var state = { selected: null };

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

    Shiny.addCustomMessageHandler("scans-app-select", function (message) {
      state.selected = message && message.id ? message.id : null;
      applySelection();
      // Keep the URL pointing at the selected trajectory so it can be shared.
      var hash = message && message.hash ? "#" + message.hash : "";
      if (hash !== window.location.hash && window.history.replaceState) {
        window.history.replaceState(
          null,
          "",
          window.location.pathname + window.location.search + hash
        );
      }
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
    });

    document.addEventListener("keydown", function (event) {
      if (event.defaultPrevented || event.altKey || event.ctrlKey || event.metaKey) {
        return;
      }
      if (isTyping(event.target)) return;
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
