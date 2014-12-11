module.exports = function(grunt){
  grunt.initConfig({

    elm: {
      main: {
        files: {
          'dist/main.js': 'elm/main.elm'
        }
      }
    },

    uglify: {
      main: {
        files: {
          'dist/main.js': 'dist/main.js'
        }
      }
    },

    less: {
      main: {
        files: {
          'dist/main.css': 'style/*.less'
        }
      }
    },

    watch: {
      options: {
        livereload: true
      },
      elm: {
        files: ['elm/**.elm'],
        tasks: 'elm'
      },
      less: {
        files: ['style/*.less'],
        tasks: 'less'
      }
    }
  });

  grunt.loadNpmTasks('grunt-contrib-less');
  grunt.loadNpmTasks('grunt-contrib-watch');
  grunt.loadNpmTasks('grunt-contrib-uglify');
  grunt.loadNpmTasks('grunt-elm');

  grunt.registerTask('devel', ['watch']);
  grunt.registerTask('make', ['less', 'elm', 'uglify']);
  grunt.registerTask('default', ['make']);
}
