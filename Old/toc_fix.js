<script>
function updateTOCPosition() {
  const hero = document.querySelector('.hero-image');
  const tocify = document.querySelector('.tocify');

  if (!hero || !tocify) {
    console.log("Hero or ToC not found");
    return;
  }

  const isHidden = hero.offsetHeight === 0 ||
                   window.getComputedStyle(hero).display === 'none' ||
                   window.getComputedStyle(hero).visibility === 'hidden';

  console.log("Hero hidden:", isHidden);
  tocify.style.top = isHidden ? '80px' : '300px';
}

document.addEventListener("DOMContentLoaded", updateTOCPosition);
window.addEventListener("resize", updateTOCPosition);

const hero = document.querySelector('.hero-image');
if (hero && typeof ResizeObserver !== 'undefined') {
  const observer = new ResizeObserver(updateTOCPosition);
  observer.observe(hero);
}
</script>