const event1 = () => {
  h.style.backgroundColor = "#eaeaea";
  header.style.backgroundColor = "#2eee12";
};
const event2 = () => {
  p.style.backgroundColor = "#ae23f0";
  section.style.backgroundColor = "#dff103";
  section.style.color = "#ffff0f";
};

const event3 = () => {
  li.style.color = "#00d0de";
};

const event4 = () => {
  li.style.backgroundColor = "#1e04f3";
  header.style.color = "#11ee22";
};

document.addEventListener("click", event1);

document.addEventListener("click", event2);

document.addEventListener("click", event3);

document.addEventListener("click", event4);
