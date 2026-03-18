// Session file load handler — wait for DOM to be ready
document.addEventListener('DOMContentLoaded', function() {
  var el = document.getElementById('load_session_hidden');
  if (el) {
    el.addEventListener('change', function() {
      var file = this.files[0];
      if (!file) return;
      Shiny.setInputValue('load_session_file', Date.now());
      var reader = new FileReader();
      reader.onload = function(e) {
        var bytes = new Uint8Array(e.target.result);
        var binary = '';
        for (var i = 0; i < bytes.length; i++) binary += String.fromCharCode(bytes[i]);
        Shiny.setInputValue('load_session_data', {name: file.name, data: btoa(binary)});
      };
      reader.readAsArrayBuffer(file);
      this.value = '';
    });
  }
});

// Read-only mode: toggle body class + bulk-disable dynamic inputs
Shiny.addCustomMessageHandler('toggle_readonly', function(msg) {
  if (msg.locked) {
    document.body.classList.add('readonly-mode');
    // Disable dynamic assign_roles inputs (renderUI-generated, can't target by shinyjs ID)
    document.querySelectorAll('#assign_roles-role_ui select, #assign_roles-role_ui input')
      .forEach(function(el) { el.disabled = true; });
  } else {
    document.body.classList.remove('readonly-mode');
    document.querySelectorAll('#assign_roles-role_ui select, #assign_roles-role_ui input')
      .forEach(function(el) { el.disabled = false; });
  }
});

// Re-apply disabled state when assign_roles UI regenerates (renderUI creates fresh inputs)
$(document).on('shiny:value', function(event) {
  if (event.name === 'assign_roles-role_ui' && document.body.classList.contains('readonly-mode')) {
    setTimeout(function() {
      document.querySelectorAll('#assign_roles-role_ui select, #assign_roles-role_ui input')
        .forEach(function(el) { el.disabled = true; });
    }, 100);
  }
});

// Linked selection indicator — update bar via custom message (no renderUI)
Shiny.addCustomMessageHandler('update_selection_bar', function(msg) {
  var bar = document.getElementById(msg.bar_id);
  var count = document.getElementById(msg.count_id);
  if (!bar || !count) return;
  if (msg.n > 0) {
    bar.style.display = 'inline-flex';
    count.textContent = msg.n + ' point' + (msg.n === 1 ? '' : 's') + ' selected';
  } else {
    bar.style.display = 'none';
  }
});
