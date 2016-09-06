<template>
  <div class="flash-message">
    <div class="alert alert-success"
         v-bind:class="{ 'show': isShown }">
      <p>{{ msg }}</p>
    </div>
  </div>
</template>

<script>
export default {
  props: ['msg'],

  data: function() {
    return {
      isShown: false
    }
  },

  methods: {
    hideMessage: function() {
      var self = this;
      setTimeout(function() {
        self.isShown = false
      }, 4000)
    },

    showMessage: function() {
      var self = this;
      setTimeout(function () {
        self.isShown = true
        self.hideMessage()
      }, 100)
    }
  },

  ready() {
    this.showMessage()
  }
}
</script>
