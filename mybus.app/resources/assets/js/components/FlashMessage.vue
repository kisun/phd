<template>
  <div class="flash-message">
    <div class="alert alert-success">
      <p>{{ msg }}</p>
    </div>
  </div>
</template>

<script>
    export default {
        ready() {
            console.log('Component ready.')
        }
    }
</script>
